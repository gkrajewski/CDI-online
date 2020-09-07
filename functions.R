#Read variable from URL and check if it is valid
readFromURL <- function(variable, session, availableValues) {
  
  value <- NULL
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query[[variable]])){
    
    value <- query[[variable]]
    
    if (!is.element(value, availableValues)){
      
      print(paste0("ERROR: Current value for parameter ", variable, " is ", value, " available values are: ", paste0(availableValues, collapse= " ")))
      CAN_START <<- FALSE

    }
    
  } else {
    
    print(paste0("ERROR: No param given in URL for variable ", variable))
    CAN_START <<- FALSE
    
  }
  
  return(value)
  
}

#Render sidebar with two buttons (previous and next) and add buttons service
renderTwoButtonsSidebar <- function(texts) {
  
  return (
    
    renderUI({
      
      list(
        
        p(class = "help-block", texts[texts$text_type == "help", "text"]),
        actionButton("prevB", label = texts[texts$text_type == "prev_button", "text"]),
        actionButton("nextB", label = texts[texts$text_type == "next_button", "text"])
        
      )
      
    })
    
  )
  
}

#Create progress bar
createProgressBar <- function(page){

  percent <- paste0(as.character(round(page/MAX_PAGE*100)),"%")
  return(div(class="progress", div(class="progress-bar", style=paste0("width: ",percent), percent)))

}

#Save data
saveData <- function(){
  
    
    ANSWERS[ANSWERS$category == CURR_CAT,"items_selected"] <<- "koko"#paste(input$items, collapse =  " ")
    ANSWERS[ANSWERS$category == CURR_CAT,"comment"] <<- "ohoho"#input$comment
    write.csv(ANSWERS, file = USER_ANSWERS_FILE, row.names = F)
    
}