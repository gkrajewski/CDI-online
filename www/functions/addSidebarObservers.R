renderPostEnd <- function(input, output){
  
  renderType(input, output, "postEnd")
  output$warning <- renderText({})
  
  for (type in types){
    
    disable(type)
    userProgress[userProgress$type == type, "disabled"] <<- TRUE
    
  } 
  
}


addSidebarObservers <- function(input, output, form, id){
  
  observeEvent(input$saveBtn, {
    
    canConfirm <- TRUE
    
    if (currType == 'start'){
      
      if (is.null(input$gender)) output$warning <- renderText({txt[txt$text_type == "noGender", "text"]})
      if (badDate(input$birthDate, form)) output$warning <- renderText({txt[txt$text_type == "badDate", "text"]})
      
      if (is.null(input$gender)){
        
        canConfirm <- FALSE
        output$warning <- renderText({txt[txt$text_type == "noGender", "text"]})
        
      } else if (badDate(input$birthDate, form)){
        
        canConfirm <- FALSE
        output$warning <- renderText({txt[txt$text_type == "badDate", "text"]})
        
      } else {
        
        userProgress[userProgress$type == 'start', "disabled"] <<- TRUE
        disable('start')
        
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
        possibleAnswers <- strsplit(typeUniqueSettings[typeUniqueSettings$type == currType, "answers_to_enable"], ",")[[1]]
        
        if(is.element(conditionedAnswer, possibleAnswers)){
          
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
          
          if (callSW(done = "true", form = form, id = id, score = countScore(form), return = FALSE)){
            
            renderPostEnd(input, output)
            
          } else {
            
            output$warning <- renderText({"Trying to connect with StarWords app"})
            delay(
              3000,
              if (callSW(done = "true", form = form, id = id, score = countScore(form), return = FALSE)){
                renderPostEnd(input, output)
              } else {
                output$warning <- renderText({"Cannot connect with StarWords app"})
              }
            )
            
          }
          
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