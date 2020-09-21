prepAnswersPattern <- function(answersPattern, currItems){
  
  if (!is.na(answersPattern)){
    
    return(strsplit(answersPattern, "")[[1]])
    
  } else {
    
    answersPattern <- list()
    
    for (i in 1:length(currItems$item_id)){
      answersPattern[[i]] <- "e"
    }
    
    return(answersPattern)
    
  }
  
}

prepComment <- function(comment){
  
  if (is.na(comment)) return("")
  return(comment)
  
}

#Create page with multiple questions and radio buttons for each question
createRadioGroup <- function(txtG, comment, items, answersPattern, choiceNames, choiceValues){
  
  #Prepare questions
  questions <- list()
  
  for (i in 1:length(items$item_id)){
    
    if (answersPattern[i] != "e"){
     
      questions[[i]] <- createRadioQuestion(items[i, "definition"], items[i, "item_id"], answersPattern[i], choiceNames, choiceValues)        
       
    } else {
      
      questions[[i]] <- createRadioQuestion(items[i, "definition"], items[i, "item_id"], character(0), choiceNames, choiceValues)      
      
    }

  }
 
  return(
    
    list(
      
      br(), br(),
      
      div(
        
        class = "multiItemsContainer",
        questions
        
      ),
      
      div(
        class = "comment",
        textAreaInput("comment", label = txtG$comment, value = comment)
      )
      
      
    )   
    
  )  
  
}