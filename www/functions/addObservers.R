addObservers <- function(input, output){

  #################################
  
  ### SIDEBAR BUTTONS OBSERVERS ###
  
  #################################
  
  #Save button
  observeEvent(input$saveBtn, {
    
    #Mark current type as done
    progress[progress$type == currType, "done"] <<- TRUE
    addClass(paste0(currType, "container"), "menuButtonContainerDone")
    
    #Get type conditioned by current type (if any)
    # conditionedType <- enableSettings[enableSettings$type == currType, "conditions"]
    conditionedTypes <- strsplit(enableSettings[enableSettings$type == currType, "conditions"], ",")[[1]]
    
    if (conditionedTypes != "none"){
      
      #Get current type answer
      conditionedAnswer <- answers[answers$type == currType, "answer"]
      
      #Get answers that can make conditioned types enabled
      possibleAnswers <- strsplit(enableSettings[enableSettings$type == currType, "answers_to_enable"], ",")[[1]]
      
      if(is.element(conditionedAnswer, possibleAnswers)){
        
        #Enable conditioned types
        for (conditionedType in conditionedTypes){
          
          progress[progress$type == conditionedType, "disabled"] <<- FALSE
          enable(conditionedType)
          
        }
        
      } else {
        
        #Disable conditioned types
        for (conditionedType in conditionedTypes){
          
          progress[progress$type == conditionedType, "disabled"] <<- TRUE
          disable(conditionedType)
          removeClass(paste0(conditionedType, "container"), "menuButtonContainerDone")
          
        }
        
      }
      
    }
    
    #Render 1st not done type (from left)
    allDone <- TRUE
    for (type in types){
      
      if (!progress[progress$type == type, "done"] & !progress[progress$type == type, "disabled"]){
        renderType(input, output, type)
        allDone <- FALSE
        break
      } 
      
    }
    
    #Render end if all types are done
    if(allDone){
      
      if (!progress[progress$type == "end", "done"]){
        
        progress[progress$type == "end", "disabled"] <<- FALSE
        enable("end")
        addClass("end", "endEnabled")
        renderType(input, output, "end")
        
      } else {
        
        renderType(input, output, "postEnd")
        
        for (type in types){
          
          disable(type)
          progress[progress$type == type, "disabled"] <<- TRUE
          
        }
        
      }
      
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
  observeEvent(input$oneCheckboxGroup, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "oneCheckboxGroup", "answer"] <<- paste(input$oneCheckboxGroup, collapse =  " ")
  })
  
  #Comment field
  observeEvent(input$comment, {
    answers[answers$type == currType & answers$category == currCat & answers$answer_type == "comment", "answer"] <<- input$comment
  })
  
  #Multi questions (max 200)
  lapply(1:200, function(i){
    
    id <- paste0("mQ", i)
    
    observeEvent(input[[id]], {

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
      
    })
    
  })
  
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