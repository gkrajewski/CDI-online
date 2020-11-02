#Return checkboxGroupInput object (takes pageItems and pageAnswer)
checkboxGroup <- function(){
  
  #Set input id (it will be used by observer)
  inputId <- "checkboxGroup"
  
  #Page answer are selected items
  selected <- pageAnswer

  #Read currently selected items
  if (!is.na(selected)){
    selected <- strsplit(selected, " ")[[1]]
  } else {
    selected <- NULL
  }
  
  #Prepare choices names and values
  choiceNames <- as.character(pageItems$definition)
  choiceValues <- as.character(pageItems$item_id)

  #Prepare checkboxGroupInput object
  checkboxes <- checkboxGroupInput(
    inputId,
    selected = selected,
    label = NULL,
    choiceNames = choiceNames,
    choiceValues = choiceValues
  )
  
  return(checkboxes)
}