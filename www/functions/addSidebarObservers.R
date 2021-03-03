confirmStart <- function(){
  
  if (nchar(idx) == 21) recurrentCallSW()
  userProgress[userProgress$type == 'start', "disabled"] <<- TRUE
  disable('start')
  return(TRUE)
  
}

addSidebarObservers <- function(input, output){
  
  observeEvent(input$saveBtn, {
    
    canConfirm <- TRUE
    
    if (currType == 'start'){
      
      if (is.null(input$gender)){
        
        canConfirm <- FALSE
        output$warning <- renderText({txt[txt$text_type == "noGender", "text"]})
        
      } else {
        
        norms <<- readNormsFile()
        
        if(!is.null(norms)){
          
          if (badDate(norms, input$birthDate)){
            
            canConfirm <- FALSE
            output$warning <- renderText({txt[txt$text_type == "badDate", "text"]})
            
          } else {

            canConfirm <- confirmStart()
            
          }
          
        } else {
          
          canConfirm <- confirmStart()
          
        }
        
      }
        
    }
    
    if (canConfirm){
      
      output$warning <- renderText({})
      
      userProgress[userProgress$type == currType, "done"] <<- TRUE
      addClass(paste0(currType, "container"), "menuButtonContainerDone")
      
      conditionedTypes <- typeUniqueSettings[typeUniqueSettings$type == currType, "conditions"]
      if (is.na(conditionedTypes)) conditionedTypes <- ""
      
      if (conditionedTypes != ""){
        
        conditionedTypes <- strsplit(conditionedTypes, ",")[[1]]
        conditionedAnswer <- answers[answers$type == currType, "answer"]
        possibleAnswers <- typeUniqueSettings[typeUniqueSettings$type == currType, "answers_to_enable"]
        if (possibleAnswers != "all") possibleAnswers <- strsplit(possibleAnswers, ",")[[1]]
        
        if(is.element(conditionedAnswer, possibleAnswers) | possibleAnswers == "all"){
          
          for (conditionedType in conditionedTypes){
            
            userProgress[userProgress$type == conditionedType, "disabled"] <<- FALSE
            enable(conditionedType)
            
          }
          
        } else {
          
          for (conditionedType in conditionedTypes){
            
            userProgress[userProgress$type == conditionedType, "disabled"] <<- TRUE
            disable(conditionedType)
            removeClass(paste0(conditionedType, "container"), "menuButtonContainerDone")
            
          }
          
        }
        
      }
      
      #Render 1st not done type (from left)
      allEnabledDone <- TRUE
      for (type in types){
        
        if (!userProgress[userProgress$type == type, "done"] & !userProgress[userProgress$type == type, "disabled"]){
          renderType(input, output, type)
          allEnabledDone <- FALSE
          break
        }
        
      }
      
      if(allEnabledDone){
        
        if (!userProgress[userProgress$type == "end", "done"]){
          
          userProgress[userProgress$type == "end", "disabled"] <<- FALSE
          enable("end")
          addClass("end", "endEnabled")
          renderType(input, output, "end")
          
        } else {
          
          renderType(input, output, "postEnd")
          
          for (type in types){
            
            disable(type)
            userProgress[userProgress$type == type, "disabled"] <<- TRUE
            
          }
          
          if (nchar(idx) == 21){
            
            score <- "false"
            if (countScore() <= readValueFromNorms()) score <- "true"
            recurrentCallSW("true", score)
            
          }
          
          write.csv(answers, answersFile, row.names = F)
          
          send.mail(
            from = "cdishiny@gmail.com",
            to = emailTo,
            subject = paste0("[SHINYDATA] ", lang, "-", form, "-", idx),
            body = "Inventory completed.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "cdishiny@gmail.com", passwd = Sys.getenv("GMAIL_PASSWORD"), ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            attach.files = c(answersFile)
          )
          
        }
        
      }      
      
    }
    
  })
  
  observeEvent(input$nextBtn, {
    
    currCat <<- categories[currCatNr + 1]
    renderCategory(input, output)
    
  })
  
  observeEvent(input$backBtn, {
    
    currCat <<- categories[currCatNr - 1]
    renderCategory(input, output)
    
  })
  
}