#Save data to file
saveData <- function(type, input, userAnswersFile, currCat, categories, inputID, sentIDs){
  
  answers <- readAnswers(type, userAnswersFile, categories, sentIDs)
  
  if (type == "word"){
    
    answers[answers$category == currCat,"items_selected"] <- paste(input$items, collapse =  " ")
    answers[answers$category == currCat,"comment"] <- input$comment
    
  } else if (type == "combine") {
    
    answers$answer <- input[[inputID]]
 
  } else if (type == "wielowyrazowe" | type == "najdluzsze") {
    
    for(sentID in sentIDs) answers[answers$sent_id == sentID, "text"] <- input[[sentID]]
    
    answers$sent_id <- unlist(answers$sent_id)
    answers$text <- unlist(answers$text)
    
  } else if (type == "phrases") {
    
    answers$items_selected <- paste(input$items, collapse =  " ")
    answers$comment <- input$comment
    
  } else {
    
    #TODO
    
  }
  
  write.csv(answers, userAnswersFile, row.names = F)
  
}