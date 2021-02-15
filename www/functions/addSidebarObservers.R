addSidebarObservers <- function(input, output, form){
  
  #Save button
  observeEvent(input$saveBtn, {
    
    if (currType == 'start' & is.null(input$gender)){
      
      #Warning message - no gender chosen
      output$warning <- renderText({txt[txt$text_type == "noGender", "text"]})
      
    } else if (currType == 'start' & badDate(input$birthDate, form)){
      
      #Warning message - bad date
      output$warning <- renderText({txt[txt$text_type == "badDate", "text"]})
      
    } else {
      
      #No warning message
      output$warning <- renderText({})

      ### CONFIRM PAGE ###

      if (currType == 'start'){

        #No return to start
        userProgress[userProgress$type == 'start', "disabled"] <<- TRUE
        disable('start')

      }

      #Count result
      if (currType == 'word') countScore(form)

      #Mark current type as done
      userProgress[userProgress$type == currType, "done"] <<- TRUE
      addClass(paste0(currType, "container"), "menuButtonContainerDone")

      #Get type conditioned by current type (if any)
      conditionedTypes <- strsplit(parts[parts$type == currType, "conditions"], ",")[[1]]

      if (conditionedTypes != "none"){

        #Get current type answer
        conditionedAnswer <- answers[answers$type == currType, "answer"]

        #Get answers that can make conditioned types enabled
        possibleAnswers <- strsplit(parts[parts$type == currType, "answers_to_enable"], ",")[[1]]

        if(is.element(conditionedAnswer, possibleAnswers)){

          #Enable conditioned types
          for (conditionedType in conditionedTypes){

            userProgress[userProgress$type == conditionedType, "disabled"] <<- FALSE
            enable(conditionedType)

          }

        } else {

          #Disable conditioned types
          for (conditionedType in conditionedTypes){

            userProgress[userProgress$type == conditionedType, "disabled"] <<- TRUE
            disable(conditionedType)
            removeClass(paste0(conditionedType, "container"), "menuButtonContainerDone")

          }

        }

      }

      #Render 1st not done type (from left)
      allEnabledDone <- TRUE
      for (type in types){

        if (!userProgress[userProgress$type == type, "done"] & !userProgress[userProgress$type == type, "disabled"]){
          renderType(input, output, type)
          allEnabledDone <- FALSE
          break
        }

      }

      if(allEnabledDone){

        if (!userProgress[userProgress$type == "end", "done"]){

          #Render end
          userProgress[userProgress$type == "end", "disabled"] <<- FALSE
          enable("end")
          addClass("end", "endEnabled")
          renderType(input, output, "end")

        } else {

          #TODO: HTTP request cdi end
          #Render post end
          renderType(input, output, "postEnd")

          for (type in types){

            disable(type)
            userProgress[userProgress$type == type, "disabled"] <<- TRUE

          }

        }

      }
      
    }
    
  })
  
  #Next button
  observeEvent(input$nextBtn, {
    
    #Set next category
    pos <- match(currCat, categories)
    currCat <<- categories[pos + 1]
    
    #Render page
    renderPage(input, output)
    
  })
  
  #Back button
  observeEvent(input$backBtn, {
    
    #Set previous category
    pos <- match(currCat, categories)
    currCat <<- categories[pos - 1]
    
    #Render page
    renderPage(input, output)
    
  })
  
}