#Read answers from file if they exists or prepare empty df      
readAnswers <- function(form, type, userAnswersFile, categories, sentIDs){
  
  if (file.exists(userAnswersFile)){
    
    return(read.csv(userAnswersFile, encoding = "UTF-8"))
    
  } else {
    
    if (type == "word" & form == "WS"){
      
      return(data.frame(category = categories, items_selected = NA, comment = NA))      
      
    } else if (type == "word" & form == "WG"){
      
      return(data.frame(category = categories, answers_pattern = NA, comment = NA))       
      
    } else if (type == "combine") {
      
      return(data.frame(answer = NA))
      
    } else if (type == "wielowyrazowe" | type == "najdluzsze"){
      
      return(data.frame(sent_id = as.array(sentIDs), text = NA))
      
    } else{
      
      return(data.frame(items_selected = NA, comment = NA))
      
    } 
    
  }
  
}
