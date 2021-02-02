addSidebarObservers <- function(input, output, form){
  
  #Save button
  observeEvent(input$saveBtn, {
    
    if (currType == 'word') countScore(form)
    
    #Mark current type as done
    progress[progress$type == currType, "done"] <<- TRUE
    addClass(paste0(currType, "container"), "menuButtonContainerDone")
    
    #Get type conditioned by current type (if any)
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
    allEnabledDone <- TRUE
    for (type in types){
      
      if (!progress[progress$type == type, "done"] & !progress[progress$type == type, "disabled"]){
        renderType(input, output, type)
        allEnabledDone <- FALSE
        break
      } 
      
    }
    
    if(allEnabledDone){
      
      if (!progress[progress$type == "end", "done"]){
        
        #Render end
        progress[progress$type == "end", "disabled"] <<- FALSE
        enable("end")
        addClass("end", "endEnabled")
        renderType(input, output, "end")
        
      } else {
        
        #TODO: HTTP request cdi end
        #Render post end 
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
  
}