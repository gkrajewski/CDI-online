#Render main panel
renderMain <- function(wd.functions, type, input, output, items, texts, userAnswersFile, txtG, form){
  
  source(paste0(wd.functions,"/saveData.R"))
  source(paste0(wd.functions,"/readAnswers.R"))
  
  if (type == "word" & form == "WS"){
    
    source(paste0(wd.functions,"/createCheckboxGroup.R"))
    multiPage <- TRUE
    
  } else if (type == "word" & form == "WG"){
    
    source(paste0(wd.functions,"/createRadioGroup.R"))
    source(paste0(wd.functions,"/createRadioQuestion.R"))
    multiPage <- TRUE
    
    choiceNames = list(
      texts[texts$text_type == "option1", "text"],
      texts[texts$text_type == "option2", "text"]
    )
    
    choiceValues = list("u", "s")
    
    currItems <- list()
    
  } else if (type == "combine"){
    
    source(paste0(wd.functions,"/createRadioQuestion.R"))
    multiPage <- FALSE
    
    #Prepare answers df
    answers <- readAnswers(form, type, userAnswersFile)
    
    #Prepare input ID
    inputID <- items$item_id
    
    if(!is.na(answers$answer)){
      selected <- answers$answer
    } else {
      selected <- character(0)
    }
    
    choiceNames = list(
      texts[texts$text_type == "option1", "text"],
      texts[texts$text_type == "option2", "text"],
      texts[texts$text_type == "option3", "text"]
    )
    
    choiceValues = list("n", "s", "o")
    
    #Add no info text when answer is changed
    observeEvent(input[[inputID]], {
      output$info <- renderText({})
    })

  } else if (type == "wielowyrazowe" | type == "najdluzsze"){
    
    multiPage <- FALSE
    
    #Create list of input fields IDs
    sentIDs <- list()
    
    if (type == "wielowyrazowe"){
      sentencesNr = 10
    } else {
      sentencesNr = 3
    } 
    
    for (i in 1:sentencesNr) sentIDs[[i]] <- paste0("s", i)
    
    #Prepare answers df
    answers <- readAnswers(form, type, userAnswersFile, sentIDs = sentIDs)
    
    #Read particular senteces and prepare text input fields
    sentences <- list()
    i <- 1
    
    for (sentID in sentIDs){
      
      sentTxt <- answers[answers$sent_id == sentID, "text"]
      
      #No NA as initial value in text input fields
      if (is.na(sentTxt)){
        sentTxt <- ""
      }
      
      sentences[[i]] <- textInput(sentID, label = NULL, value = sentTxt)
      i <- i + 1
      
    }
    
    #No text about 'answers saved' when answer is changed
    lapply(1:sentencesNr, function(i) {
      
      sentID <- paste0("s",i)
      
      observeEvent(input[[sentID]],{
        output$info <- renderText({})
      })
      
    })
    
  } else {
    
    ### Page with multiple checkboxes ###
    
    source(paste0(wd.functions,"/createCheckboxGroup.R"))
    multiPage <- FALSE
    
    #Add no info text when answer is changed
    observeEvent(input$items, {
      output$info <- renderText({})
    })
    
    observeEvent(input$comment, {
      output$info <- renderText({})
    })
    
  }
  
  if (multiPage){
    
    source(paste0(wd.functions,"/createProgressBar.R"))
    currPage <- reactiveVal(0)
    categories <- unique(items$category)
    pageNr <- length(categories) + 1
    
    #Add next button service
    observeEvent(input$nextB, {
      
      nextPage <- currPage() + 1
      if (nextPage <= pageNr){
        
        #Save data if on current page are input fields
        if (currPage() != 0) saveData(form, type, input, userAnswersFile, categories[currPage()], categories, items)
        
        #Change page
        currPage(nextPage)
        
      } 
      
    })
    
    #Add previous button service
    observeEvent(input$prevB, {
      
      nextPage <- currPage() - 1
      if (nextPage >= 0 ){
        
        #Save data if on current page are input fields
        if (currPage() != pageNr) saveData(form, type, input, userAnswersFile, categories[currPage()], categories, items) 
        
        #Change page
        currPage(nextPage)
        
      } 
      
    }) 
    
  } else {
    
    #Add submit button service
    observeEvent(input$submit, {
      
      saveData(form, type, input, userAnswersFile, inputID = inputID, sentIDs = sentIDs)
      output$info <- renderText({txtG$info})
      
    })
    
  }
  
  return(

    renderUI({

      if (multiPage){

        list(

          #Render progress bar
          createProgressBar(currPage(),pageNr),

          if (currPage() == 0){

            #Render first page instruction
            texts[texts$text_type == "instr", "text"]

          } else if (currPage() == pageNr) {

            #Render last page thanks
            txtG$thanks

          } else {

            ### Render input page ###

            #Get item category for current page
            currCat <- categories[currPage()]

            #Prepare answers df
            answers <- readAnswers(form, type, userAnswersFile, categories)
            
            #Prepare comment so it can be used in rendering UI
            comment <- prepComment(answers[answers$category == currCat, "comment"])

            #Create suitable input fields
            if (form == "WS"){
              
              #Prepare info about items so it can be used in rendering UI
              selected <- prepItems(answers[answers$category == currCat, "items_selected"])
              choiceNames <- as.character(items[items$category == currCat, ]$definition)
              choiceValues <- as.character(items[items$category == currCat, ]$item_id)
              
              list(
                
                h4(texts[texts$text_type == currCat, "text"]),
                createCheckboxGroup(txtG, selected, choiceNames, choiceValues, comment) 
                
              )
             
            } else {
              
              currItems <- items[items$category == currCat, ]
              answersPattern <- prepAnswersPattern(answers[answers$category == currCat, "answers_pattern"], currItems)
              
              list(
                
                h4(texts[texts$text_type == currCat, "text"]),
                br(),
                texts[texts$text_type == "instr2", "text"],
                br(),
                createRadioGroup(txtG, comment, currItems, answersPattern, choiceNames, choiceValues) 
                
              )
              
            }#end else

          }#end else

        )#end list

      } else {

        if (type == "combine"){
          
          #Create a question with radio buttons
          createRadioQuestion(items$definition, inputID, selected, choiceNames, choiceValues)          
          
        } else if (type == "wielowyrazowe" | type == "najdluzsze") {
          
          #Render a page with instruction and text input fields
          list(
            
            texts[texts$text_type == "instr", "text"],
            br(), br(),
            
            if (sentencesNr != 3){
              
              div(
                
                class = "sentencesContainer",
                sentences
                
              )          
              
            } else {
              
              sentences
              
            }
            
          )
          
        } else {
          
          answers <- readAnswers(form, type, userAnswersFile)
          comment <- prepComment(answers$comment)
          selected <- prepItems(answers$items_selected)
          choiceNames <- as.character(items$definition)
          choiceValues <- as.character(items$item_id)
          
          list(
            
            texts[texts$text_type == "instr", "text"],
            createCheckboxGroup(txtG, selected, choiceNames, choiceValues, comment)
            
          )
          
        }#end else 

      }#end else

    })#end renderUI

  )#end return
  
}#end renderMain