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
  
  if ("groups" %in% txt$text_type) {
    
    #Many groups form
    groups<-strsplit(txt[txt$text_type == "groups", "text"], ",")[[1]]
    
  } else {
    
    #One group form
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
  
  groupsToTestBool <- vector(, length(groups))
  
  for (i in 1:length(groups)) {
    groupsToTestBool[i] <- is.na(subject[[paste0(groups[i], "Test")]])
  }
  
  groupsToTest <- groups[groupsToTestBool]
  groupsToTest <- sample(groupsToTest)
  
  values <- reactiveValues()
  values$groupIdx <- 1
  values$subgroup <- groupsToTest[values$groupIdx]
  values$commentGroup <- groupsToTest[values$groupIdx]
  values$itemsGroup <- items[items$group==values$subgroup, ]
  values$subject <- subject
  values$groupsToSave <- c()
  values$sendLogs <- TRUE
  
  CATdesign <- prepareGroup(output = output, 
                            input = input, 
                            values = values,
                            txt = txt, 
                            startThetas = startThetas, 
                            subjectAge = subjectAge, 
                            urlString = urlString)
  

  #Save CAT design and subject to file when session ends
  CATdesign <- reactiveVal(CATdesign)
  session$onSessionEnded(function() {
    
    saveRDS(isolate(CATdesign()), isolate(values$designFile))
    saveRDS(isolate(values$subject), subjectFile)
    
    toSave = isolate(values$groupsToSave)
    for (saveblock in toSave) {
      
      answerFile <- paste0("designs/", urlString, "-", saveblock, ".csv")
      outputTable <- read.csv(answerFile)
      
      if (txt[txt$text_type == "email", "text"]=="yes") {
        loginfo(paste0(urlString, "-", saveblock, " sending email."))
        sendMail(subjectText=paste0("[SHINYDATA] ", urlString, "-", saveblock),
                 txt="Inventory completed.",
                 id=paste0(urlString, " group=", saveblock),
                 host="smtp.gmail.com",
                 port=465,
                 username=MAIL_USERNAME,
                 password=Sys.getenv("GMAIL_PASSWORD"),
                 recipients=EMAILS_RECIPIENTS,
                 attach=answerFile
        )
      } else {
        loginfo(paste0(urlString, " sending emails disabled."))
      }
      
      
      tableName <- paste0("form_", form, "_", lang, "_adaptive")
      query = paste0("CREATE TABLE `", Sys.getenv("DB_NAME"), "`.`",tableName,"` (
                            `idx` VARCHAR(45) NULL,
                            `gender` VARCHAR(45) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `birth` VARCHAR(45) NULL,
                            `test` VARCHAR(45) NULL,
                            `start_theta` FLOAT NULL,
                            `filler` VARCHAR(45) NULL,
                            `lang` VARCHAR(45) NULL,
                            `group` VARCHAR(45) NULL,
                            `q_id` INT NOT NULL,
                            `items` VARCHAR(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `answers` INT NULL,
                            `comment` VARCHAR(", toString(STRING_LIMIT), ") CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `theta` FLOAT NULL,
                            `se_theta` FLOAT NULL,
                            `final` INT NULL,
                            `start_date` DATETIME NULL,
                            `end_date` DATETIME NULL);")
      
      sendDatabase(username=Sys.getenv("DB_USERNAME"),
                                   password=Sys.getenv("DB_PASSWORD"),
                                   dbname=Sys.getenv("DB_NAME"),
                                   host=Sys.getenv("DB_HOST"),
                                   port=Sys.getenv("DB_PORT"),
                                   id=paste0(urlString, " group=", saveblock),
                                   tableName=tableName,
                                   tableCreate=query,
                                   tableInput=outputTable)
    }
    
    if (isolate(values$sendLogs)) {
       sendLogs(urlString, idx, form, lang)
   }
    
  })
  
  #Add comment saving
  observeEvent(input$comment, {
    values$subject[[paste0(values$commentGroup, "Comment")]] <- input$comment
  })
  
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
      answerFile <- paste0("designs/", urlString, "-", isolate(values$subgroup), ".csv")
      write.csv(outputTable, answerFile, row.names = F)
      
      values$subject[[paste0(values$subgroup, "Test")]] <- "end"
      loginfo(paste0(urlString, " done with part", values$groupIdx))
      
      values$groupsToSave <- c(values$groupsToSave, isolate(values$subgroup))
      
      if (values$groupIdx==length(groupsToTest)) {
        labelBtn <- txt[txt$text_type == "endBtn", "text"]
        completeEnd <- TRUE
      } else {
        labelBtn <- txt[txt$text_type == "continueBtn", "text"]
        completeEnd <- FALSE
      }
      
      output$main <- renderUI({
        list(
          if (completeEnd) h5(txt[txt$text_type == "endText", "text"]),
          div(class = "comment", textAreaInput("comment", label = txt[txt$text_type == "commentLabel", "text"], value = ""))
        )
      })
      
      output$sidebar <- renderUI({
        actionButton("commentBtn", label = labelBtn, class = "btn-primary")
      })
      
      observeEvent(input$commentBtn, {
        
        #Save comment to csv
        answerFile <- paste0("designs/", urlString, "-", isolate(values$commentGroup), ".csv")
        outputTable <- read.csv(answerFile)
        outputTable$comment <- values$subject[[paste0(values$commentGroup, "Comment")]]
        write.csv(outputTable, answerFile, row.names = F)
        
        #Update comment group
        values$commentGroup <- values$subgroup
        
        if (completeEnd) {
          
          values$subject[["formEnded"]] <- TRUE
          
          showModal(modalDialog(
            title = txt[txt$text_type == "end", "text"],
            txt[txt$text_type == "thanks", "text"],
            easyClose = FALSE,
            footer = NULL
          ))
          
          CATdesign(updatedDesign)
          saveRDS(isolate(CATdesign()), isolate(values$designFile))
          saveRDS(isolate(values$subject), subjectFile)
          recurrentCallSW(idx, form, lang, done = "true", score="true")
          
          toSave = isolate(values$groupsToSave)
          for (saveblock in toSave) {
            
            answerFile <- paste0("designs/", urlString, "-", saveblock, ".csv")
            outputTable <- read.csv(answerFile)
            
            if (txt[txt$text_type == "email", "text"]=="yes") {
              loginfo(paste0(urlString, "-", saveblock, " sending email."))
              sendMail(subjectText=paste0("[SHINYDATA] ", urlString, "-", saveblock),
                       txt="Inventory completed.",
                       id=paste0(urlString, " group=", saveblock),
                       host="smtp.gmail.com",
                       port=465,
                       username=MAIL_USERNAME,
                       password=Sys.getenv("GMAIL_PASSWORD"),
                       recipients=EMAILS_RECIPIENTS,
                       attach=answerFile
              )
            } else {
              loginfo(paste0(urlString, " sending emails disabled."))
            }
            
            tableName <- paste0("form_", form, "_", lang, "_adaptive")
            query = paste0("CREATE TABLE `", Sys.getenv("DB_NAME"), "`.`",tableName,"` (
                            `idx` VARCHAR(45) NULL,
                            `gender` VARCHAR(45) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `birth` VARCHAR(45) NULL,
                            `test` VARCHAR(45) NULL,
                            `start_theta` FLOAT NULL,
                            `filler` VARCHAR(45) NULL,
                            `lang` VARCHAR(45) NULL,
                            `group` VARCHAR(45) NULL,
                            `q_id` INT NOT NULL,
                            `items` VARCHAR(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `answers` INT NULL,
                            `comment` VARCHAR(", toString(STRING_LIMIT), ") CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `theta` FLOAT NULL,
                            `se_theta` FLOAT NULL,
                            `final` INT NULL,
                            `start_date` DATETIME NULL,
                            `end_date` DATETIME NULL);")
            
            sendDatabase(username=Sys.getenv("DB_USERNAME"),
                         password=Sys.getenv("DB_PASSWORD"),
                         dbname=Sys.getenv("DB_NAME"),
                         host=Sys.getenv("DB_HOST"),
                         port=Sys.getenv("DB_PORT"),
                         id=paste0(urlString, " group=", saveblock),
                         tableName=tableName,
                         tableCreate=query,
                         tableInput=outputTable)
            
            
            values$groupsToSave <- values$groupsToSave[values$groupsToSave!=saveblock]
          }
          
          sendLogs(urlString, idx, form, lang)
          values$sendLogs <- FALSE
        
        } else {
          
          ### NOT TEST DEFINITIVE END - ONLY GROUP END ###
          
          saveRDS(isolate(CATdesign()), isolate(values$designFile))
          
          values$subject[[paste0(values$subgroup, "Test")]] <- "end"
          
          values$groupIdx <- values$groupIdx +1
          values$subgroup <- groupsToTest[values$groupIdx]
          values$itemsGroup <- items[items$group==values$subgroup, ]
          
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
    
}