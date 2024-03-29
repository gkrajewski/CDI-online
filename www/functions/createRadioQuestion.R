createRadioQuestion <- function(questionId, choiceNames, choiceValues, selected, questionLabel = NULL, inline = F, noBreakInside = TRUE){
  
  if (is.na(selected) | selected == 0) selected <- character(0)
  
  choiceNames <- lapply(choiceNames, function(x) HTML(x))
  
  if (!is.null(questionLabel)){
    questionLabel = HTML(questionLabel)
  }
  
  radioGroup <- (
    radioButtons(
      questionId,
      label = questionLabel,
      selected = selected,
      choiceNames = choiceNames,
      choiceValues = choiceValues,
      inline = inline
    )
  )
  
  if (noBreakInside) radioGroup <- div(class="noBreakInside", radioGroup)
  return(radioGroup)
  
}