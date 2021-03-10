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

createRadioQuestion <- function(questionId, choiceNames, choiceValues, selected, questionLabel = NULL, inline = F, noBreakInside = TRUE){

  if (is.na(selected) | selected == 0) selected <- character(0)
  
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

renderInput <- function(){
  
  questions <- list()
  
  if (inputType == "radio" | inputType == "manyCheckboxGroups" | inputType == "radioAlt" | inputType == "checkboxAlt"){

    if (is.na(catAnswer)){
      selected <- c(rep("0,", nrow(catItems)))
    } else {
      selected <- strsplit(catAnswer, ",")[[1]]
    }
    
    if (inputType != "radioAlt" & inputType != "checkboxAlt"){
      
      choiceNames <- strsplit(catTxt[catTxt$text_type == "choiceNames", "text"], ",")[[1]]
      choiceValues <- c(1 : length(choiceNames))
      
    } 

    for (i in 1:nrow(catItems)){
      
      if (inputType == "radio"){
        
        questions[[i]] <- createRadioQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], catItems[i, "definition"], T)
        
      } else if (inputType == "radioAlt"){
        
        choiceNames <-  strsplit(catItems[i, "definition"], "%")[[1]]
        choiceValues <- c(1 : length(choiceNames))
        questions[[i]] <- list(createRadioQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i]), br())
        
      } else if (inputType == "manyCheckboxGroups") {
        
        questions[[i]] <- createCheckboxQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], catItems[i, "definition"], T, noBreakInside = TRUE)
        
      } else if (inputType == "checkboxAlt") {
        
        choiceNames <-  strsplit(catItems[i, "definition"], "%")[[1]]
        choiceValues <- c(1 : length(choiceNames))
        questions[[i]] <- list(createCheckboxQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], noBreakInside = FALSE), br())
        
      }
      
    }
    
  } else if (inputType == "oneCheckboxGroup"){
    
    if (is.na(catAnswer)) catAnswer <- 0
    choiceNames <- as.character(catItems$definition)
    choiceValues <- c(1 : nrow(catItems))
    questions[[1]] <- createCheckboxQuestion(inputType, choiceNames, choiceValues, catAnswer, noBreakInside = FALSE)
    
  } else if (inputType == "sentences"){
    
    if (currCat != ""){
      sentencesNr <<- as.numeric(substr(currCat, 10, nchar(currCat)))
    } else {
      sentencesNr <<- as.numeric(substr(currType, 10, nchar(currType)))
    }
    
    if (is.na(catAnswer)) sentences <- c(rep("%", sentencesNr))   
    sentences <- strsplit(catAnswer, "%")[[1]]
    
    for (i in 1 : sentencesNr){
      questions[[i]] <- textInput(paste0("s", i), label = NULL, value = sentences[i])
    }
    
  } else if (inputType == "demographic"){
    
    if (is.na(catAnswer)){
      
      currGender <- character(0)
      currDate <- NULL
      currFiller <- character(0)
      currFillerTxt <- ""
      
    } else {
      
      catAnswer <- strsplit(catAnswer, ",")[[1]]
      currDate <- catAnswer[1]
      currGender <- catAnswer[2]
      currFiller <- catAnswer[3]
      currFillerTxt <- catAnswer[4]
      
    }
    
    questions[[1]] <- br()
    questions[[2]] <- dateInput("birthDate", txt[txt$text_type == "dateLabel", "text"], currDate, language = lang)
    questions[[3]] <- br()
    questions[[4]] <- radioButtons(
      "gender",
      label = txt[txt$text_type == "genderLabel", "text"],
      selected = currGender,
      choiceNames = strsplit(txt[txt$text_type == "genders", "text"], ",")[[1]],
      choiceValues = c("female", "male", "other")
    )
    questions[[5]] <- br()
    questions[[6]] <- radioButtons(
      "filler",
      label = txt[txt$text_type == "fillerLabel", "text"],
      selected = currFiller,
      choiceNames = strsplit(txt[txt$text_type == "fillers", "text"], ",")[[1]],
      choiceValues = c("mother", "father", "grandmother", "grandfather", "nanny", "teacher", "other")
    )
    questions[[7]] <- textInput("fillerTxt", label = NULL, value = currFillerTxt)
    
  }
    
  return(div(id="currInput", questions))
  
}