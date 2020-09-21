#Save data to file
saveData <- function(form, type, input, userAnswersFile, currCat, categories, items, inputID, sentIDs){
  
  answers <- readAnswers(form, type, userAnswersFile, categories, sentIDs)
  
  if (type == "word" & form == "WS"){
    
    answers[answers$category == currCat,"items_selected"] <- paste(input$items, collapse =  " ")
    answers[answers$category == currCat,"comment"] <- input$comment
    
  } else if (type == "word" & form == "WG"){
    
    i <- 1
    currAnswers <- list()
    currItems <- items[items$category == currCat, "item_id"]
    
    for (item in currItems) {
      
      if (!is.null(input[[item]])){
        
        currAnswers[[i]] <- input[[item]]
        
      } else {
        
        currAnswers[[i]] <- "e"
      }
      
      i <- i + 1
      
    }
    
    answers[answers$category == currCat,"answers_pattern"] <- paste0(currAnswers, collapse = "")
    answers[answers$category == currCat,"comment"] <- input$comment
    
  } else if (type == "combine") {
    
    answers$answer <- input[[inputID]] #TODO
 
  } else if (type == "wielowyrazowe" | type == "najdluzsze") {
    
    for(sentID in sentIDs) answers[answers$sent_id == sentID, "text"] <- input[[sentID]]
    
    answers$sent_id <- unlist(answers$sent_id)
    answers$text <- unlist(answers$text)
    
  } else {
    
    answers$items_selected <- paste(input$items, collapse =  " ")
    answers$comment <- input$comment
    
  }
  
  write.csv(answers, userAnswersFile, row.names = F)
  
}