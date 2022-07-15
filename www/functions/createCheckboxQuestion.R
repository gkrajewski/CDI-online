createCheckboxQuestion <- function(questionId, choiceNames, choiceValues, selected, questionLabel = NULL, inline = F, noBreakInside){
  
  if (selected == 0){
    selected <- NULL
  } else {
    selected <- strsplit(selected, " ")[[1]]
  }
  
  choiceNames <- lapply(choiceNames, function(x) HTML(x))
  
  checkboxGroup <- checkboxGroupInput(
    questionId,
    label = questionLabel,
    selected = selected,
    choiceNames = choiceNames,
    choiceValues = choiceValues,
    inline = inline
  )
  
  if (noBreakInside) checkboxGroup <- div(class="noBreakInside", checkboxGroup)
  return(checkboxGroup)
  
}