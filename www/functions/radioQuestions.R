createRadioQuestion <- function(questionId, questionTxt, choiceNames, choiceValues, selected){
  
  if (selected == 0) selected <- character(0)
  
  return(
    
    fluidRow(
      
      column(
        
        8,
        align = "left",
        questionTxt
        
      ),
      
      column(
        
        4,
        align = "center",
        
        radioButtons(
          
          questionId,
          label = NULL,
          selected = selected,
          choiceNames = choiceNames,
          choiceValues = choiceValues,
          inline = T
          
        )
        
      )#end column
      
    )#end fluidRow    
    
  )#end return
  
}

#Return list of questions with radio buttons (takes pageItems, pageAnswer and pageTxt)
radioQuestions <- function(){
  
  #Get number of items
  itemsNr <- length(pageItems$item_id)
  
  #Prepare choice names
  choiceNames <- strsplit(pageTxt[pageTxt$text_type == "choiceNames", "text"], ",")[[1]]
  
  #Prepare choice values
  choiceValues <- c(1 : length(choiceNames))
  
  #Prepare selected answers list
  if (is.na(pageAnswer)){
    selected <- c(rep(0,itemsNr))   
  } else {
    selected <- strsplit(pageAnswer, "")[[1]]
  }
  
  #Prepare questions
  questions <- list()
  
  #Fill in questions list
  for (i in 1:length(pageItems$item_id)){
    
    questions[[i]] <- createRadioQuestion(paste0("rQ", i), pageItems[i, "definition"], choiceNames, choiceValues, selected[i])        

  }
  
  return(questions)

}