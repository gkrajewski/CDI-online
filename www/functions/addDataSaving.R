saveData <- function(input){
  
  if (inputType == "radio" | inputType == "manyCheckboxGroups" | inputType == "radioAlt" | inputType == "checkboxAlt"){
    
    answersPattern <- c()
    
    for (i in 1 : nrow(catItems)){
      
      id <- paste0("mQ", i)
      
      if (!is.null(input[[id]])){
        answer <- paste0(input[[id]], collapse = " ")
        answersPattern[[i]] <- answer
      } else {
        answersPattern[[i]] <- 0
      }
      
    }
    
    answersPattern <- paste0(answersPattern, collapse = ",")
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == inputType, "answer"] <<- answersPattern
    
  }
  
}

addDataSaving <- function(input){
  
  observeEvent(input$oneCheckboxGroup, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "oneCheckboxGroup", "answer"] <<- paste(input$oneCheckboxGroup, collapse =  " ")
  })
  
  observeEvent(input$comment, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "comment", "answer"] <<- input$comment
  })
  
  demographic <- reactive({paste(input$birthDate, input$gender, input$filler, input$fillerTxt)})
  observeEvent(demographic(), {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "demographic", "answer"] <<- paste0(input$birthDate, ",", input$gender, ",", input$filler, ",", input$fillerTxt)
  })
  
  onclick("currInput",
    saveData(input)
  )
  
  lapply(1:10, function(i){
    
    id <- paste0("s", i)
    
    observeEvent(input[[id]], {
      
      sentences <- c()
      
      for (i in 1 : sentencesNr){
        
        id <- paste0("s", i)
        
        if (!is.null(input[[id]])){
          sentences[[i]] <- input[[id]]
        } else {
          sentences[[i]] <- ""
        }
        
      }
      
      sentences <- paste0(sentences, collapse = "%")
      answers[answers$type == currType & answers$category == currCat & answers$answer_type == "sentences", "answer"] <<- sentences
      
    })
    
  })
  
}