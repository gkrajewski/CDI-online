addObservers <- function(input, output){

  #################################
  
  ### SIDEBAR BUTTONS OBSERVERS ###
  
  #################################
  
  #Save button
  observeEvent(input$saveBtn, {
    
    #Mark current type as done
    progress[progress$type == currType, "done"] <<- TRUE
    addClass(paste0(currType, "container"), "menuButtonContainerDone")
    
    #Disable current type
    progress[progress$type == currType, "disabled"] <<- TRUE
    disable(currType)
    
    #Get type conditioned by current type (if any)
    conditionedType <- enableSettings[enableSettings$type == currType, "conditions"]
    
    if (conditionedType != "none"){
      
      #Get current type answer
      conditionedAnswer <- answers[answers$type == currType, "answer"]
      print(conditionedAnswer)
      
      #Get answers that can make conditioned type enabled
      possibleAnswers <- strsplit(enableSettings[enableSettings$type == currType, "answers_to_enable"], ",")[[1]]
      
      if(is.element(conditionedAnswer, possibleAnswers)){
        
        #Enable conditioned type
        progress[progress$type == conditionedType, "disabled"] <<- FALSE
        enable(conditionedType)
        
      }
      
    }
    
    #Render 1st enabled type (from left)
    allDisabled <- TRUE
    for (type in types){
      if (!progress[progress$type == type, "disabled"]){
        renderType(input, output, type)
        allDisabled <- FALSE
        break
      } 
    }
    
    #Render end type if all types are disabled (so they are done)
    if(allDisabled){
      progress[progress$type == "end", "disabled"] <<- FALSE
      enable("end")
      addClass("end", "endEnabled")
      renderType(input, output, "end")
    }     
      
  })
  
  #Next button
  observeEvent(input$nextBtn, {
    
    #Get next category
    if (currCat != "none"){
      pos <- match(currCat, categories)
      currCat <<- categories[pos + 1]
    } else {
      currCat <<- categories[1]
    }
    
    #Render page
    renderPage(input, output)
    
  })
  
  #Back button
  observeEvent(input$backBtn, {
    
    #Get previous category
    pos <- match(currCat, categories)
    if (pos > 1){
      currCat <<- categories[pos - 1]
    } else {
      currCat <<- "none"
    }
    
    #Render page
    renderPage(input, output)
    
  })
  
  #############################################
  
  ### INPUT OBJECTS OBSERVERS (DATA SAVING) ###
  
  #############################################
  
  #Checkbox group
  observeEvent(input$checkboxGroup, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "checkboxGroup", "answer"] <<- paste(input$checkboxGroup, collapse =  " ")
  })
  
  #Comment field
  observeEvent(input$comment, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "comment", "answer"] <<- input$comment
  })
  
  #Radio questions (max 100)
  lapply(1:100, function(i){
    
    id <- paste0("rQ", i)
    
    observeEvent(input[[id]], {

      answersPattern <- c()
      
      for (i in 1 : nrow(pageItems)){
        
        id <- paste0("rQ", i)
        
        if (!is.null(input[[id]])){
          answersPattern[[i]] <- input[[id]]
        } else {
          answersPattern[[i]] <- 0
        }
        
      }
      
      answersPattern <- paste0(answersPattern, collapse = "")
      answers[answers$type == currType & answers$category == currCat & answers$answer_type == "radioQuestions", "answer"] <<- answersPattern
      
    })
    
  })
  
  #Sentences (max 10)
  lapply(1:10, function(i){
    
    id <- paste0("s", i)
    
    observeEvent(input[[id]], {
      
      sentences <- c()
      
      for (i in 1 : pageSettings$sentences_nr){
        
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