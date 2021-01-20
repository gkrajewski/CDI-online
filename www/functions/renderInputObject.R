createCheckboxQuestion <- function(questionId, choiceNames, choiceValues, selected, questionLabel = NULL, inline = F, noBreakInside){
  
  if (selected == 0){
    selected <- NULL
  } else {
    selected <- strsplit(selected, " ")[[1]]
  }
      
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

createRadioQuestion <- function(questionId, choiceNames, choiceValues, selected, questionLabel = NULL, inline = F){

  if (selected == 0) selected <- character(0)
  
  return(
      
    radioButtons(
      questionId,
      label = questionLabel,
      selected = selected,
      choiceNames = choiceNames,
      choiceValues = choiceValues,
      inline = inline
    )
    
  )
  
}

renderInputObject <- function(){
  
  questions <- list()
  
  if (pageInputType == "radio" | pageInputType == "manyCheckboxGroups"){

    if (is.na(pageAnswer)){
      selected <- c(rep("0,", nrow(pageItems)))
    } else {
      selected <- strsplit(pageAnswer, ",")[[1]]
    }

    choiceNames <- strsplit(pageTxt[pageTxt$text_type == "choiceNames", "text"], ",")[[1]]
    choiceValues <- c(1 : length(choiceNames))

    for (i in 1:nrow(pageItems)){
      
      if (pageInputType == "radio"){
        questions[[i]] <- createRadioQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], pageItems[i, "definition"], T)
      } else {
        questions[[i]] <- createCheckboxQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], pageItems[i, "definition"], T, noBreakInside = TRUE)
      }
      
    }
    
  } else if (pageInputType == "oneCheckboxGroup"){
    
    # if (is.na(pageAnswer)){
    #   selected <- c(rep("0,", nrow(pageItems)))
    # } else {
    #   selected <- strsplit(pageAnswer, ",")[[1]]
    # }
    #
    
    if (is.na(pageAnswer)) pageAnswer <- 0
    choiceNames <- as.character(pageItems$definition)
    choiceValues <- c(1 : nrow(pageItems))
    questions[[1]] <- createCheckboxQuestion(pageInputType, choiceNames, choiceValues, pageAnswer, noBreakInside = FALSE)
    
  } else if (pageInputType == "sentences"){
    
    sentencesNr <- pageSettings$sentences_nr
    if (is.na(pageAnswer)) sentences <- c(rep("%", sentencesNr))   
    sentences <- strsplit(pageAnswer, "%")[[1]]
    
    for (i in 1 : sentencesNr){
      questions[[i]] <- textInput(paste0("s", i), label = NULL, value = sentences[i])
    }
    
  } else if (pageInputType == "radioAlt"){
    
    if (is.na(pageAnswer)){
      selected <- c(rep("0,", nrow(pageItems)))
    } else {
      selected <- strsplit(pageAnswer, ",")[[1]]
    }
    
    for (i in 1:nrow(pageItems)){
      
      choiceNames <-  strsplit(pageItems[i, "definition"], "%")[[1]]
      choiceValues <- c(1 : length(choiceNames))
      questions[[i]] <- list(createRadioQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i]), br())
      
    }
    
  }
  
  return(questions)
  
}