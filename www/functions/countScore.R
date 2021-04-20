countScore <- function(answers, typeUniqueSettings, type = "word", answersCounted = 1){
  
  typeAnswers <- answers[answers$type == type,]
  typeInputType <- typeUniqueSettings[typeUniqueSettings$type == type, "input_type"]
  rawAnswers <- typeAnswers[typeAnswers$answer_type == typeInputType, "answer"]
  
  if (typeInputType == "manyCheckboxGroups"){
    
    answersStrings <- rawAnswers
    answerString <- paste0(na.omit(answersStrings), collapse = ", ")
    splittedAnswer <- strsplit(answerString[1], ",")[[1]]
    numericAnswer <- as.numeric(splittedAnswer)
    replaced <- replace(numericAnswer, is.na(numericAnswer), 2)
    # tbl <- table(replaced)
    # print(tbl)
    
    score <- sum(replaced == answersCounted)
    if (type == "word" & answersCounted == 1){
      score <- score + sum(replaced == 2)
    }
    
  } else if (typeInputType == "oneCheckboxGroup"){
    
    answersInt <- na.omit(rawAnswers)
    answersStrings <- as.character(answersInt)
    str <- paste0(answersStrings, collapse = " ")
    score <- length(strsplit(str, " ")[[1]])
    
  }
  
  return(score)
    
}