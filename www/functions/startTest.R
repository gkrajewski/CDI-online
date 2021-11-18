startTest <- function(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, urlString){

  #Get subject (children) age in months
  subjectAge <- interval(subject$birth, Sys.Date()) %/% months(1)
  
  #Load items and start thetas
  setwd(testPath)
  items <- read.csv(paste0("items.csv"), encoding = "UTF-8", strip.white = T)
  startThetasFile <- "startThetas.csv"
  if (file.exists(startThetasFile)) {
    startThetas <- read.csv(paste0("startThetas.csv"), encoding = "UTF-8", strip.white = T, dec=",")
  } else {
    startThetas <- NA
  }
  setwd(INIT_PATH)
  
  #Prepare parts settings
  if ("groups" %in% txt$text_type) {
    
    #Many parts form
    groups<-strsplit(txt[txt$text_type == "groups", "text"], ",")[[1]]
    
  } else {
    
    #One part form
    groups <- c("group1")
    items$group <- "group1"
    txt[txt$text_type == "MirtMethod", "text_type"] = paste0("group1", txt[txt$text_type == "MirtMethod", "text_type"])
    txt[txt$text_type == "MirtCriteria", "text_type"] = paste0("group1", txt[txt$text_type == "MirtCriteria", "text_type"])
    
    if ("MirtSeTheta" %in% txt$text_type) {
      txt[txt$text_type == "MirtSeTheta", "text_type"] = paste0("group1", txt[txt$text_type == "MirtSeTheta", "text_type"])
    }
    if ("maxItemNr" %in% txt$text_type) {
      txt[txt$text_type == "maxItemNr", "text_type"] = paste0("group1", txt[txt$text_type == "maxItemNr", "text_type"])
    }
    if ("minItemNr" %in% txt$text_type) {
      txt[txt$text_type == "minItemNr", "text_type"] = paste0("group1", txt[txt$text_type == "minItemNr", "text_type"])
    }
    
  }
  
  #Prepare vector of groups tested already or not
  groupsToTestBool <- vector(, length(groups))
  
  for (i in 1:length(groups)) {
    groupsToTestBool[i] <- is.na(subject[[paste0(groups[i], "Test")]])
  }
  
  #Sample some parts order from parts that were not started already
  groupsToTest <- groups[groupsToTestBool]
  groupsToTest <- sample(groupsToTest)
  
  #Prepare reactive variables
  values <- reactiveValues()
  values$groupIdx <- 1
  values$subgroup <- groupsToTest[values$groupIdx]
  values$commentGroup <- groupsToTest[values$groupIdx]
  values$itemsGroup <- items[items$group==values$subgroup, ]
  values$subject <- subject
  values$groupsToSave <- c()
  values$sendLogs <- TRUE
  
  #Prepare CAT design for given part
  CATdesign <- prepareGroup(output = output, 
                            input = input, 
                            values = values,
                            txt = txt, 
                            startThetas = startThetas, 
                            subjectAge = subjectAge, 
                            urlString = urlString)
  
  CATdesign <- reactiveVal(CATdesign)
  
  #Add comment saving
  observeEvent(input$comment, {
    values$subject[[paste0(values$commentGroup, "Comment")]] <- input$comment
  })
  
  #Wait for question response
  observeEvent(input$question, {
    
    #Update design
    updatedDesign <- updateDesign(CATdesign(), new_item = values$nextItem, new_response = input$question)

    if ((length(na.omit(updatedDesign$person$items_answered)) >= values$minItemNr) &
        ((length(na.omit(updatedDesign$person$items_answered)) >= values$maxItemNr) |
        (updatedDesign$person$thetas_SE_history[length(updatedDesign$person$thetas_SE_history)]<
         values$seTheta))
    ){
      
      ### PART END ###
      
      endDate <- Sys.time()
      outputTable <- prepareOutputAdaptative(isolate(CATdesign()), isolate(values$itemsGroup$item) ,isolate(values$subject), lang, isolate(values$subgroup), endDate)
      answerFile <- paste0("answers/", urlString, "-", isolate(values$subgroup), ".csv")
      write.csv(outputTable, answerFile, row.names = F)
      
      values$subject[[paste0(values$subgroup, "Test")]] <- "end"
      loginfo(paste0(urlString, " done with part", values$groupIdx))
      
      values$groupsToSave <- c(values$groupsToSave, isolate(values$subgroup))
      
      if (values$groupIdx==length(groupsToTest)) {
        btnLabel <- txt[txt$text_type == "endBtn", "text"]
        completeEnd <- TRUE
      } else {
        btnLabel <- txt[txt$text_type == "continueBtn", "text"]
        completeEnd <- FALSE
      }
      
      output$main <- renderUI({
        list(
          if (completeEnd) h5(txt[txt$text_type == "endText", "text"]),
          div(class = "comment", textAreaInput("comment", label = txt[txt$text_type == "commentLabel", "text"], value = ""))
        )
      })
      
      output$sidebar <- renderUI({
        actionButton("partEndBtn", label = btnLabel, class = "btn-primary")
      })
      
      observeEvent(input$partEndBtn, {
        
        #Save comment
        answerFile <- paste0("answers/", urlString, "-", isolate(values$commentGroup), ".csv")
        outputTable <- read.csv(answerFile)
        outputTable$comment <- values$subject[[paste0(values$commentGroup, "Comment")]]
        write.csv(outputTable, answerFile, row.names = F)
        
        #Update comment group
        values$commentGroup <- values$subgroup
        
        if (completeEnd) {
          
          ### FORM COMPLETE END ###
          
          values$subject[["formEnded"]] <- TRUE
          
          showModal(modalDialog(
            title = txt[txt$text_type == "end", "text"],
            txt[txt$text_type == "thanks", "text"],
            easyClose = FALSE,
            footer = NULL
          ))
          
          CATdesign(updatedDesign)

          recurrentCallSW(idx, form, lang, done = "true", score="true")
          
          saveCAT(
            CATdesign = isolate(CATdesign()),
            designFile = isolate(values$designFile),
            subject = isolate(values$subject),
            subjectFile = subjectFile,
            groupsToSave = isolate(values$groupsToSave),
            txt = txt,
            urlString = urlString,
            form = form,
            lang = lang,
            sendLogs = isolate(values$sendLogs),
            idx = idx
          )
          
          values$groupsToSave <- c()
          values$sendLogs <- FALSE
        
        } else {
          
          ### NOT FORM COMPLETE END - ONLY PART END ###
          
          saveRDS(isolate(CATdesign()), isolate(values$designFile))
          
          values$subject[[paste0(values$subgroup, "Test")]] <- "end"
          values$groupIdx <- values$groupIdx +1
          values$subgroup <- groupsToTest[values$groupIdx]
          values$itemsGroup <- items[items$group==values$subgroup, ]
          
          #Prepare CAT design for new part
          CATdesign <- prepareGroup(output = output, 
                                    input = input, 
                                    values = values,
                                    txt = txt, 
                                    startThetas = startThetas, 
                                    subjectAge = subjectAge, 
                                    urlString = urlString)
          
          CATdesign(CATdesign)
          
        }
        
      }, once = TRUE, ignoreInit = TRUE)


    } else {
      
      # Render next item
      values$nextItem <- findNextItem(updatedDesign)
      updateRadioButtons(session, "question",
                         label = paste0(values$itemsGroup$question[values$nextItem], ' "', values$itemsGroup$item[values$nextItem], '"?'),
                         selected = character(0))

      CATdesign(updatedDesign)
      
    }
    
  })
  
  #Save ended parts when session ends
  session$onSessionEnded(function() {
    
    saveCAT(
      CATdesign = isolate(CATdesign()),
      designFile = isolate(values$designFile),
      subject = isolate(values$subject),
      subjectFile = subjectFile,
      groupsToSave = isolate(values$groupsToSave),
      urlString = urlString,
      txt = txt,
      form = form,
      lang = lang,
      sendLogs = isolate(values$sendLogs),
      idx = idx
    )
    
  })
    
}