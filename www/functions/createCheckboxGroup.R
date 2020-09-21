prepItems <- function(selectedStr){
  
  if (!is.na(selectedStr)) return(strsplit(selectedStr, " ")[[1]])
  return(NULL)
  
}

prepComment <- function(comment){
  
  if (is.na(comment)) return("")
  return(comment)
  
}

#Create page with multiple checkboxes and comment field
createCheckboxGroup <- function(txtG, selected, choiceNames, choiceValues, comment){
  
  checkboxGroup <- checkboxGroupInput(
    "items",
    selected = selected,
    label = NULL,
    choiceNames = choiceNames,
    choiceValues = choiceValues
  )
  
  if (length(choiceNames) > 5){
    
    checkboxGroup <- div(
      class = "multiItemsContainer",
      checkboxGroup
    )
    
  } else {
    
    checkboxGroup <- div(
      class = "fewItemsContainer",
      checkboxGroup
    )
    
  }
  
  return(
    
    list(
      
      br(), br(),
      
      checkboxGroup,
      
      div(
        class = "comment",
        textAreaInput("comment", label = txtG$comment, value = comment)
      )
      
    )   
    
  )
  
}