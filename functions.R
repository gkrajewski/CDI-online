#Read variable from URL and check if it is valid
readFromURL <- function(parameter, session, availableValues) {
  
  value <- NULL
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query[[parameter]])){
    
    value <- query[[parameter]]
    
    if (!is.element(value, availableValues)){
      
      cat(paste0("\nERROR: Current value for parameter: ", parameter, " is: ", value, "\nAvailable values are: ", paste0(availableValues, collapse= " ")))
      START <<- FALSE
    }
    
  } else {
    
    cat(paste0("\nERROR: No value given in URL for parameter: ", parameter))
    START <<- FALSE
  }
  
  return(value)
  
}


#Create progress bar depending on ratio page/maximum page
createProgressBar <- function(maxPage){
  
  percent <- paste0(as.character(round(PAGE/maxPage*100)),"%")
  return(div(class="progress", div(class="progress-bar", style=paste0("width: ",percent), percent)))
  
}


#Render main panel depending on page
renderMain <- function(texts, items, input, maxPage, categories, userAnswersFile){
  
  if (PAGE == 0) {
    
    #Welcome page
    INPUT_PAGE <<- FALSE
    
    return(
      
      renderUI({
        
        list(
          
          createProgressBar(maxPage),
          texts[texts$text_type == "instr", "text"]
          
        )
        
      })
      
    )
    
  } else if (PAGE == maxPage) {
    
    #Thanks page
    INPUT_PAGE <<- FALSE
    
    return(
      
      renderUI({
        
        list(
          
          createProgressBar(maxPage),
          texts[texts$text_type == "thanks", "text"]
          
        )

      })
      
    )
    
  } else {
    
    #Input page
    INPUT_PAGE <<- TRUE
    
    currCat<- categories[PAGE]
    selectedStr <- as.character(ANSWERS[ANSWERS$category == currCat, "items_selected"])
    comment <- ANSWERS[ANSWERS$category == currCat, "comment"]
    
    if (!is.na(selectedStr)){
      selected <- strsplit(selectedStr, " ")[[1]]
    } else {
      selected <- NULL
    }
    
    if (is.na(comment)){
      comment = ""
    }
    
    return(
      
      renderUI({
        
        list(
          
          createProgressBar(maxPage),
          h4(texts[texts$text_type == currCat, "text"]),
          
          br(),
          
          div(
            
            class = "itemsContainer",
            
            checkboxGroupInput(
              "items",
              selected = selected,
              label = NULL,
              choiceNames = as.character(items[items$category == currCat, ]$definition),
              choiceValues = as.character(items[items$category == currCat, ]$item_id)
            )
            
          ),
          
          div (
            
            class = "commentContainer",
            
            textAreaInput("comment", label = texts[texts$text_type == "comment", "text"], value = comment)
            
          )
          
        )#end list        
        
      })#end renderUI
      
    )#end return

  }#end else

}#end renderMain


#Save data
saveData <- function(userAnswersFile, input, categories){
  
  print("Wezwano mn")
  currCat <- categories[PAGE]
  
  ANSWERS[ANSWERS$category == currCat,"items_selected"] <<- paste(input$items, collapse =  " ")
  ANSWERS[ANSWERS$category == currCat,"comment"] <<- input$comment
  write.csv(ANSWERS, userAnswersFile, row.names = F)
  
}


#Render UI with multi pages
renderMultiPageUI <- function(texts, items, input, output, categories, userAnswersFile) {
  
  #Render sidebar
  output$sidebar <- renderUI({
    
    list(
      
      p(class = "help-block", texts[texts$text_type == "help", "text"]),
      actionButton("prevB", label = texts[texts$text_type == "prev_button", "text"]),
      actionButton("nextB", label = texts[texts$text_type == "next_button", "text"])
      
    )
    
  })
  
  #Add button clicks observers
  PAGE <<- 0
  maxPage <- length(categories) + 1
  
  observeEvent(input$nextB, {
    
    print(INPUT_PAGE)
    if (PAGE < maxPage){
      
      if (INPUT_PAGE) saveData(userAnswersFile, input, categories)
      PAGE <<- PAGE + 1
      output$main <- renderMain(texts, items, input, maxPage, categories, userAnswersFile)

    }
    
  })
  
  observeEvent(input$prevB, {
    
    print(INPUT_PAGE)
    if (PAGE > 0){
      
      if (INPUT_PAGE) saveData(userAnswersFile, input, categories)
      PAGE <<- PAGE - 1
      output$main <- renderMain(texts, items, input, maxPage, categories, userAnswersFile)

    }
    
  })
  
  #Render main panel
  output$main <- renderMain(texts, items, input, maxPage, categories, userAnswersFile)

}