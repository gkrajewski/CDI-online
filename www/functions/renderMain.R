#Render main panel
renderMain <- function(wd.functions, type, input, output, items, texts, userAnswersFile){
  
  source(paste0(wd.functions,"/saveData.R"))
  source(paste0(wd.functions,"/readAnswers.R"))
  
  if (type == "word"){
    
    source(paste0(wd.functions,"/createCheckboxGroup.R"))
    multiPage <- TRUE
    
  } else if (type == "combine"){
    
    source(paste0(wd.functions,"/createRadioQuestion.R"))
    multiPage <- FALSE
    
    #Prepare answers df
    answers <- readAnswers(type, userAnswersFile)
    
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
    answers <- readAnswers(type, userAnswersFile, sentIDs = sentIDs)
    
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
    
    #TODO: (gesty, naśladownictwo itd.)
    
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
        if (currPage() != 0) saveData(type, input, userAnswersFile, categories[currPage()], categories)
        
        #Change page
        currPage(nextPage)
        
      } 
      
    })
    
    #Add previous button service
    observeEvent(input$prevB, {
      
      nextPage <- currPage() - 1
      if (nextPage >= 0 ){
        
        #Save data if on current page are input fields
        if (currPage() != pageNr) saveData(type, input, userAnswersFile, categories[currPage()], categories)
        
        #Change page
        currPage(nextPage)
        
      } 
      
    }) 
    
  } else {
    
    #Add submit button service
    observeEvent(input$submit, {
      
      saveData(type, input, userAnswersFile, inputID = inputID, sentIDs = sentIDs)
      output$info <- renderText({texts[texts$text_type == "info", "text"]})
      
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
            texts[texts$text_type == "thanks", "text"]

          } else {

            ### Render input page ###

            #Get item category for current page
            currCat <- categories[currPage()]

            #Prepare answers df
            answers <- readAnswers(type, userAnswersFile, categories)

            #Prepare answers so they can be used in output
            selectedStr <- answers[answers$category == currCat, "items_selected"]
            comment <- answers[answers$category == currCat, "comment"]

            if (!is.na(selectedStr)){
              selected <- strsplit(selectedStr, " ")[[1]]
            } else {
              selected <- NULL
            }

            if (is.na(comment)){
              comment <- ""
            }

            #Create suitable input fields
            if (type == "word"){
              
              createCheckboxGroup(texts, currCat, selected, items, comment)              
              
            } else {
              
              #TODO (gesty)
              
            }

          }

        )

      } else {

        if (type == "combine"){
          
          #Create a question with radio buttons to answer
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
          
          #TODO (naśladownictwo itp.)
          
        }

      }#end else

    })#end renderUI

  )#end return
  
}#end renderMain