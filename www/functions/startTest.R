startTest <- function(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, parameters, urlString, fromSW){

  #Get subject (children) age in months
  subjectAge <- interval(subject$birth, Sys.Date()) %/% months(1)
  loginfo(paste0("Subject Age: ", subjectAge))
  
  #Load items and start thetas
  setwd(testPath)
  inputFilesRead <- tryCatch(
    expr = {
      items <- read.csv(paste0("items.csv"), encoding = "UTF-8", strip.white = T)
      startThetasFile <- "startThetas.csv"
      if (file.exists(startThetasFile)) {
        startThetas <- read.csv(paste0("startThetas.csv"), encoding = "UTF-8", strip.white = T, dec=",")
      } else {
        startThetas <- NA
      }
      loginfo(" Input files were read in")
    },
    error = function(m){
      msg <- paste0(" There is problem with input files <br><br>", m)
      logerror(msg)
      output$sidebar <- renderText({msg})
      return(FALSE)
    }
  )
  
  setwd(INIT_PATH)

  #Prepare parts settings
  if ("groups" %in% parameters$parameter) {
    
    #Many parts form
    groups<-strsplit(parameters[parameters$parameter=="groups", "value"], ",")[[1]]
    
    loginfo(paste0(" Following groups are defined: ", paste0(groups, collapse = ", ")))
    
  } else {
    
    #One part form
    groups <- c("group1")
    items$group <- "group1"
    parameters[parameters$parameter=="MirtMethod", "parameter"] = paste0("group1", parameters[parameters$parameter=="MirtMethod", "parameter"])
    parameters[parameters$parameter=="MirtCriteria", "parameter"] = paste0("group1", parameters[parameters$parameter=="MirtCriteria", "parameter"])
    
    if ("MirtSeTheta" %in% parameters$parameter) {
      parameters[parameters$parameter=="MirtSeTheta", "parameter"] = paste0("group1", parameters[parameters$parameter=="MirtSeTheta", "parameter"])
    }
    if ("maxItemNr" %in% parameters$parameter) {
      parameters[parameters$parameter=="maxItemNr", "parameter"] = paste0("group1", parameters[parameters$parameter=="maxItemNr", "parameter"])
    }
    if ("minItemNr" %in% parameters$parameter) {
      parameters[parameters$parameter=="minItemNr", "parameter"] = paste0("group1", parameters[parameters$parameter=="minItemNr", "parameter"])
    }
    
    loginfo(" Groups parameter was not defined")
  }
  
  #Prepare vector of parts tested already and not
  groupsToTestBool <- vector(, length(groups))
  startGroup <- c()
  
  start = "items"

  for (i in 1:length(groups)) {
    
    groupsToTestBool[i] <- FALSE
    
    if (is.na(subject[[paste0(groups[i], "Test")]])) {
      
      if (!is.na(subject[[paste0(groups[i], "Start")]])) {
        
        startGroup <- groups[i]
        
        loginfo(paste0(" This is the start group: ", startGroup))
        
      } else {
        
        groupsToTestBool[i] <- TRUE
        
      }
    } else if (is.na(subject[[paste0(groups[i], "CommentEnd")]])) {
      
      startGroup <- groups[i]
      start = "comment"
      
    }
  }

  #Sample some parts order from parts that were not started already
  groupsToTest <- groups[groupsToTestBool]
  groupsToTest <- c(startGroup, sample(groupsToTest))
  
  loginfo(paste0(" This is the group order: ", paste0(groupsToTest, collapse=", ")))

  #Prepare reactive variables
  values <- reactiveValues()
  values$groupIdx <- 1
  values$subgroup <- groupsToTest[values$groupIdx]
  values$itemsGroup <- items[items$group==values$subgroup, ]
  values$subject <- subject
  values$groupsToSave <- c()
  values$sendLogs <- TRUE
  
  if (start=="comment") {
    
    loginfo(paste0(" Starting part ", values$subgroup, " with comment"))
    
    values$designFile <- designFile <- paste0("CATdesigns/", urlString, "-", values$subgroup, ".rds")
    CATdesign <- readRDS(isolate(values$designFile))
    
    loginfo(paste0(" Design for group ", values$subgroup, " was read in"))
    
    if (values$groupIdx==length(groupsToTest)) {
      btnLabel <- txt[txt$text_type == "endBtn", "text"]
      values$completeEnd <- TRUE
      loginfo(paste0(" All parts of the form were completed. GroupIdx: ", isolate(values$groupIdx), 
                     " groups in total: ", length(groupsToTest)))
    } else {
      btnLabel <- txt[txt$text_type == "continueBtn", "text"]
      values$completeEnd <- FALSE
      loginfo(paste0(" Not all parts were completed. GroupIdx: ", isolate(values$groupIdx), 
              " groups in total: ", length(groupsToTest)))
    }
    
    commentInput = ""
    if (!is.na(values$subject[[paste0(values$subgroup, "Comment")]])) commentInput = values$subject[[paste0(values$subgroup, "Comment")]]
    
    header <- paste0(isolate(values$subgroup), "Header")
    headerColor <- paste0(isolate(values$subgroup), "HeaderColor")
    
    output$main <- renderUI({
      list(
        if (header %in% txt$text_type & headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"], style=paste0("color: ", txt[txt$text_type == headerColor, "text"], ";")),
        if (header %in% txt$text_type & !headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"]),
        if (values$completeEnd) h5(txt[txt$text_type == "endText", "text"]),
        div(class = "comment", textAreaInput("comment", label = txt[txt$text_type == "commentLabel", "text"], value = commentInput))
      )
    })
    
    output$sidebar <- renderUI({
      actionButton(paste0("partEndBtn", values$subgroup), label = btnLabel, class = "btn-primary")
    })
    
  } else {
    
    loginfo(paste0(" Starting part ", isolate(values$subgroup), " with items"))
    #Prepare CAT design for given part
    CATdesign <- prepareGroup(output = output, 
                              input = input, 
                              values = values,
                              txt = txt, 
                              parameters = parameters,
                              startThetas = startThetas, 
                              subjectAge = subjectAge, 
                              urlString = urlString)
    
    loginfo(paste0(" Created CATdesign for ", isolate(values$subgroup)))
  }
  
  CATdesign <- reactiveVal(CATdesign)
  
  #Add comment saving
  observeEvent(input$comment, {
    values$subject[[paste0(values$subgroup, "Comment")]] <- input$comment
  })
  
  #Wait for question response
  observeEvent(input$question, {
    
    #Update design
    updatedDesign <- updateDesign(CATdesign(), new_item = values$nextItem, new_response = input$question)

    values$items_answered = length(na.omit(updatedDesign$person$items_answered))
    
    if ((values$items_answered >= values$minItemNr) &
        ((values$items_answered >= values$maxItemNr) |
        (updatedDesign$person$thetas_SE_history[length(updatedDesign$person$thetas_SE_history)]<
         values$seTheta))
    ){
      
      ### PART END ###
      CATdesign(updatedDesign)
      values$subject[[paste0(values$subgroup, "Test")]] <- "end"
      values$groupsToSave <- c(values$groupsToSave, isolate(values$subgroup))
      loginfo(paste0(" done with part ", isolate(values$groupIdx), " group: ", isolate(values$subgroup), 
                     " groups to save: ", paste0(isolate(values$groupsToSave), collapse=", ")))
      loginfo(paste0(" Items answered ", isolate(values$items_answered), 
                     " se theta: ", updatedDesign$person$thetas_SE_history[length(updatedDesign$person$thetas_SE_history)]))
      
      #Save data
      endDate <- Sys.time()
      outputTable <- prepareOutputAdaptative(isolate(CATdesign()), isolate(values$itemsGroup$id) ,isolate(values$subject), lang, isolate(values$subgroup), endDate)
      loginfo(paste0(" prepared output file for the group ", isolate(values$subgroup)))
      answerFile <- paste0("answers/", urlString, "-", isolate(values$subgroup), ".csv")
      write.csv(outputTable, answerFile, row.names = F)
      
      if (values$groupIdx==length(groupsToTest)) {
        btnLabel <- txt[txt$text_type == "endBtn", "text"]
        values$completeEnd <- TRUE
        loginfo(paste0(" All parts of the form were completed. GroupIdx: ", isolate(values$groupIdx), 
                       " groups in total: ", length(groupsToTest)))
      } else {
        btnLabel <- txt[txt$text_type == "continueBtn", "text"]
        values$completeEnd <- FALSE
        loginfo(paste0(" Not all parts were completed. GroupIdx: ", isolate(values$groupIdx), 
                       " groups in total: ", length(groupsToTest)))
      }
      
      header <- paste0(isolate(values$subgroup), "Header")
      headerColor <- paste0(isolate(values$subgroup), "HeaderColor")
      
      output$main <- renderUI({
        list(
          if (header %in% txt$text_type & headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"], style=paste0("color: ", txt[txt$text_type == headerColor, "text"], ";")),
          if (header %in% txt$text_type & !headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"]),
          if (values$completeEnd) h5(txt[txt$text_type == "endText", "text"]),
          div(class = "comment", textAreaInput("comment", label = txt[txt$text_type == "commentLabel", "text"], value = ""))
        )
      })
      
      output$sidebar <- renderUI({
        actionButton(paste0("partEndBtn", values$subgroup), label = btnLabel, class = "btn-primary")
      })

    } else {
      
      # Render next item
      values$nextItem <- findNextItem(updatedDesign)
      #updateRadioButtons(session, "question",
      #                   label = paste0(values$itemsGroup$question[values$nextItem], ' ', values$itemsGroup$item[values$nextItem], '?'),
      #                   selected = character(0))

      CATdesign(updatedDesign)
      
    }
    
  })
  
  # Action after ending one part of the test
  partEndActions <- function() {
    
    loginfo(paste0(" Part ", isolate(values$subgroup), " was ended"))
    
    #Comment part ended
    values$subject[[paste0(values$subgroup, "CommentEnd")]] <- "end"
    
    #Save comment
    answerFile <- paste0("answers/", urlString, "-", isolate(values$subgroup), ".csv")
    outputTable <- read.csv(answerFile)
    outputTable$comment <- values$subject[[paste0(values$subgroup, "Comment")]]
    write.csv(outputTable, answerFile, row.names = F)
    
    if (values$completeEnd) {
      
      loginfo(" Form was ended")
      
      ### FORM COMPLETE END ###
      
      values$subject[["formEnded"]] <- TRUE
      
      endMsgtxt <- txt[txt$text_type == "thanksMsgText", "text"]
      additionalMessage <- parameters[parameters$parameter=="additionalEndMessageFromDatabase", "value"]
      
      if (additionalMessage=="yes") {
        additionalMessageTxt <- getAdditionalEndMessage(urlString, "database", parameters, txt)
        endMsgtxt <- paste(endMsgtxt, "<br><br>", additionalMessageTxt)
      }
      
      #Prepare redirection
      if ("redirectionURL" %in% parameters$parameter){
        
        footer <- list(
          actionButton("redirect", txt[txt$text_type == "redirectionBtn", "text"], class = "btn-primary"),
          div(id="redirectionText", txt[txt$text_type == "redirectionText", "text"])
        )
        
        observeEvent(input$redirect, {
          redirect(parameters, idx, lang, form, "adaptive")
        }, once = TRUE)
        
      } else {
        
        footer <- NULL
        
      }
      
      #Show ending message
      showModal(modalDialog(
        title = txt[txt$text_type == "thanksMsgTitle", "text"],
        HTML(endMsgtxt),
        easyClose = FALSE,
        footer = footer
      ))
      
      #Disable redirection button for now (if created)
      if ("redirectionURL" %in% parameters$parameter) disable("redirect")
      
      #Call SW
      if (fromSW) recurrentCallSW(idx, form, lang, done = "true", score="true")
      
      saveCAT(
        CATdesign = isolate(CATdesign()),
        designFile = isolate(values$designFile),
        subject = isolate(values$subject),
        subjectFile = subjectFile,
        groupsToSave = isolate(values$groupsToSave),
        urlString = urlString,
        parameters = parameters,
        form = form,
        lang = lang,
        sendLogs = isolate(values$sendLogs),
        idx = idx
      )
      
      values$groupsToSave <- c()
      values$sendLogs <- FALSE
      
      #Enable redirection button (if created)
      if ("redirectionURL" %in% parameters$parameter) enable("redirect")
      
    } else {
      
      ### NOT FORM COMPLETE END - ONLY PART END ###
      saveRDS(isolate(CATdesign()), isolate(values$designFile))
      
      values$groupIdx <- values$groupIdx +1
      values$subgroup <- groupsToTest[values$groupIdx]
      values$itemsGroup <- items[items$group==values$subgroup, ]
      
      loginfo(paste0(" Another group starting. Group: ", isolate(values$subgroup), " group Idx: ", isolate(values$groupIdx)))
      
      #Prepare CAT design for new part
      CATdesign <- prepareGroup(output = output, 
                                input = input, 
                                values = values,
                                txt = txt, 
                                parameters = parameters,
                                startThetas = startThetas, 
                                subjectAge = subjectAge, 
                                urlString = urlString)
      
      CATdesign(CATdesign)
      
    }
    
  }
  
  
  for (i in groupsToTest){
    observeEvent(input[[paste0("partEndBtn", values$subgroup)]], {
      partEndActions()
    }, once=TRUE, ignoreInit = TRUE)
  }
  
  #Save ended parts when session ends
  session$onSessionEnded(function() {
    
    saveCAT(
      CATdesign = isolate(CATdesign()),
      designFile = isolate(values$designFile),
      subject = isolate(values$subject),
      subjectFile = subjectFile,
      groupsToSave = isolate(values$groupsToSave),
      urlString = urlString,
      parameters = parameters,
      form = form,
      lang = lang,
      sendLogs = isolate(values$sendLogs),
      idx = idx
    )
    
  })
    
}