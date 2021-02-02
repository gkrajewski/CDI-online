countScore <- function(form){
  
  if (form == "WG"){
    
    answersStrings <- answers[answers$answer_type == "manyCheckboxGroups", "answer"]
    answerString <- paste0(na.omit(answersStrings), collapse = ", ")
    splittedAnswer <- strsplit(answerString[1], ",")[[1]] #1?
    numericAnswer <- as.numeric(splittedAnswer)
    replaced <- replace(numericAnswer, is.na(numericAnswer), 2)
    tbl <- table(replaced)
    score1 <- sum(replaced == 1)
    score2 <- sum(replaced == 2)
    print(score1)
    print(score2)
    
  } else {
    
    answersInt <- na.omit(answers[answers$answer_type == "oneCheckboxGroup", "answer"])
    answersStrings <- as.character(answersInt)
    str <- paste0(answersStrings, collapse = " ")
    score <- length(strsplit(str, " ")[[1]])
    print(score)
    
  }
  
}