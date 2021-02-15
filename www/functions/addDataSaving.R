addDataSaving <- function(input, output){
  
  #One group of checkboxes
  observeEvent(input$oneCheckboxGroup, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "oneCheckboxGroup", "answer"] <<- paste(input$oneCheckboxGroup, collapse =  " ")
  })
  
  #Comment field
  observeEvent(input$comment, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "comment", "answer"] <<- input$comment
  })
  
  #Demographic input
  demographic <- reactive({paste(input$birthDate, input$gender)})
  observeEvent(demographic(), {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == pageInputType, "answer"] <<- paste0(input$birthDate, ",", input$gender)
    
    if (!is.null(input$birthDate)){
      # print(input$birthDate)
      # diff <- Sys.Date() - as.Date(input$birthDate)
      # print(as.integer(diff))
      age <- interval(input$birthDate, Sys.Date()) %/% months(1)
      print(age)
      
    }
    
  })
  
  #Many radio buttons or many groups of checkboxes
  onclick("currInput", 
          
    if (pageInputType == "radio" | pageInputType == "manyCheckboxGroups" | pageInputType == "radioAlt"){
      
      answersPattern <- c()
      
      for (i in 1 : nrow(pageItems)){
        
        id <- paste0("mQ", i)
        
        if (!is.null(input[[id]])){
          answer <- paste0(input[[id]], collapse = " ")
          answersPattern[[i]] <- answer
        } else {
          answersPattern[[i]] <- 0
        }
        
      }
      
      answersPattern <- paste0(answersPattern, collapse = ",")
      answers[answers$type == currType & answers$category == currCat & answers$answer_type == pageInputType, "answer"] <<- answersPattern
      
    } 
          
  )
  
  #Sentences (max 10)
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