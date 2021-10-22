startTest <- function(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, urlString){

  loginfo(paste0(urlString, "--- START TEST ---"))
  
  #TODO
  #Select current test for subject
  if (is.na(subject[["testEnd"]])){
    subjectAge <- interval(subject$birth, Sys.Date()) %/% months(1)
    subjectGender <- subject$gender
    subject$test <- form
  } 
  
  #Load items and start thetas
  setwd(testPath)
  items <- read.csv(paste0("items.csv"), encoding = "UTF-8", strip.white = T)
  startThetasFile <- paste0("startThetas.csv")
  if (file.exists(startThetasFile)) {
    startThetas <- read.csv(paste0("startThetas.csv"), encoding = "UTF-8", strip.white = T, dec=",")
  } else {
    startThetas <- NA
  }
  setwd(INIT_PATH)
  
  if ("groups" %in% txt$text_type) {
    groups<-strsplit(txt[txt$text_type == "groups", "text"], ",")[[1]]
  } else {
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
  
  chooseTheta <- function(startThetas, subjectGender, subjectAge, group) {
    if (!is.data.frame(get("startThetas"))) {
      return(0)
    } 
    
    if (!("group" %in% colnames(startThetas)) & !("age" %in% colnames(startThetas)) & !("gender" %in% colnames(startThetas))) {
      return(0)
    }
    
    if ("group" %in% colnames(startThetas)) {
      if (group %in%  startThetas$group) {
        groupFile = startThetas[startThetas$group==group, ]
      } else {
        return(0)
      }
    } else {
        groupFile=startThetas
    }
      
    if ("gender" %in% colnames(groupFile)) {
      if (subjectGender %in% groupFile$gender) {
        startThetasFile = groupFile[groupFile$gender==subjectGender, ]
      } else {
        if ("age" %in% colnames(groupFile)) {
          startThetasFile = groupFile %>% group_by(age) %>% summarise(theta=mean(theta))
        } else {
          return(0)
        }
      }
    } else {
      if ("age" %in% colnames(groupFile)) {
        startThetasFile = groupFile %>% group_by(age) %>% summarise(theta=mean(theta))
      } else {
        return(0)
      }
    }
    
    if ("age" %in% colnames(startThetasFile)) {
      #choose the closest age provided in StartThetasFile to the subject age
      ageDiff=abs(startThetasFile$age-subjectAge)
      idAge = which.min(ageDiff) 
      startTheta <- startThetasFile$theta[idAge]
    } else {
      startTheta <- startThetasFile[startThetasFile["gender"]==subjectGender, "theta"][1]
    }
    
    return(startTheta)
  }
  
  groupsToTestBool <- vector(, length(groups))
  
  for (i in 1:length(groups)) {
    groupsToTestBool[i] <- is.na(subject[[paste0(groups[i], "Test")]])
  }
  
  groupsToTest <- groups[groupsToTestBool]
  
  groupIdx <- 1
  subgroup <- groupsToTest[groupIdx]

  itemsGroup <- items[items$group==subgroup, ]
  
  #Prepare start theta
  if (is.na(subject[[paste0(subgroup, "Theta")]])){
    subject[[paste0(subgroup, "Theta")]] <- chooseTheta(startThetas, subjectGender, subjectAge, subgroup)
  }

  #Prepare mirtCAT design object
  designFile <- paste0("designs/", urlString, "-", subgroup, ".rds")
  if (file.exists(designFile)){
    CATdesign <- readRDS(designFile)
  } else {
    params <- itemsGroup[, c("a1", "d")]
    mirt_object <- generate.mirt_object(params, '2PL')
    CATdesign <- mirtCAT(mo = mirt_object, 
                         method = txt[txt$text_type == paste0(subgroup, "MirtMethod"), "text"], 
                         criteria = txt[txt$text_type == paste0(subgroup, "MirtCriteria"), "text"], 
                         start_item = txt[txt$text_type == paste0(subgroup, "MirtCriteria"), "text"], 
                         design_elements = TRUE, 
                         design = list(thetas.start = subject[[paste0(subgroup, "Theta")]]))
  }

  #Render sidebar instruction
  output$sidebar <- renderUI({
    div(class = "help-block", txt[txt$text_type == "testInstr", "text"])
  })
  
  #Adjust sidebar background color
  

  #Render first question
  if (is.na(subject[[paste0(subgroup, "Start")]])) {
    subject[[paste0(subgroup, "Start")]] <- as.character(Sys.time())
  }
  values <- reactiveValues()
  values$nextItem <- findNextItem(CATdesign)
  values$groupIdx <- groupIdx
  values$subgroup <- subgroup
  values$itemsGroup <- itemsGroup
  values$designFile <- designFile
  values$subject <- subject
  values$groupsToSave <- c()
  
  #Render question and optionally header
  header <- paste0(isolate(values$subgroup), "Header")
  headerColor <- paste0(isolate(values$subgroup), "HeaderColor")
  output$main <- renderUI({
    list(
      if (header %in% txt$text_type & headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"], style=paste0("color: ", txt[txt$text_type == headerColor, "text"], ";")),
      if (header %in% txt$text_type & !headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"]),
      radioButtons(
        "question",
        label = paste0(values$itemsGroup$question[values$nextItem], ' "', values$itemsGroup$item[values$nextItem], '"?'),
        selected = character(0),
        choiceNames = strsplit(txt[txt$text_type == "choiceNames", "text"], ",")[[1]],
        choiceValues = c(0,1)
      )
    )
  })

  #Set maximum number of items in test (stop criterion)
  if (paste0(isolate(values$subgroup), "maxItemNr") %in% txt$text_type) {
    values$maxItemNr <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "maxItemNr"), "text"])
  } else {
    values$maxItemNr <- nrow(itemsGroup)
  }
  
  if (isolate(values$maxItemNr)>nrow(itemsGroup)) {
    values$maxItemNr <- nrow(itemsGroup)
  }
  
  #Set minimum number of items in test 
  if (paste0(isolate(values$subgroup), "minItemNr") %in% txt$text_type) {
    values$minItemNr <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "minItemNr"), "text"])
  } else {
    values$minItemNr <- 0
  }

  if (isolate(values$minItemNr)>isolate(values$maxItemNr) | isolate(values$minItemNr)>nrow(itemsGroup)) {
    stop("Minimum item number greater than maximum or available number of items")
  }
  
  #Set se_theta value 
  if (paste0(isolate(values$subgroup), "MirtSeTheta") %in% txt$text_type) {
    values$seTheta <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "MirtSeTheta"), "text"])
  } else {
    values$seTheta <- 0
  }

  #Render progress bar
  #createProgressBar(output, CATdesign, maxItemNr)
  
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
                            `theta` FLOAT NULL,
                            `se_theta` FLOAT NULL,
                            `final` INT NULL,
                            `start_date` DATETIME NULL,
                            `end_date` DATETIME NULL);")
      
      sendDatabase <- sendDatabase(username=Sys.getenv("DB_USERNAME"),
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
  })

  observeEvent(input$question, {
    
    #Update design
    updatedDesign <- updateDesign(CATdesign(), new_item = values$nextItem, new_response = input$question)

    #Update progress bar
    #createProgressBar(output, updatedDesign, maxItemNr)

    if ((length(na.omit(updatedDesign$person$items_answered)) >= values$minItemNr) &
        ((length(na.omit(updatedDesign$person$items_answered)) >= values$maxItemNr) |
        (updatedDesign$person$thetas_SE_history[length(updatedDesign$person$thetas_SE_history)]<
         values$seTheta))
    ){
      
      endDate <- Sys.time()
      outputTable <- prepareOutputAdaptative(isolate(CATdesign()), isolate(values$itemsGroup$item) ,isolate(values$subject), lang, isolate(values$subgroup), endDate)
      answerFile <- paste0("designs/", urlString, "-", isolate(values$subgroup), ".csv")
      write.csv(outputTable, answerFile, row.names = F)
      
      loginfo(paste0(urlString, " done with part", values$groupIdx))
      
      values$groupsToSave <- c(values$groupsToSave, isolate(values$subgroup))
      
      if (values$groupIdx==length(groupsToTest)) {
        
        ### TEST DEFINITIVE END ###
        
        values$subject[["testEnd"]] <- "end"
        values$subject[[paste0(values$subgroup, "Test")]] <- "end"
        
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
                            `theta` FLOAT NULL,
                            `se_theta` FLOAT NULL,
                            `final` INT NULL,
                            `start_date` DATETIME NULL,
                            `end_date` DATETIME NULL);")

          sendDatabase <- sendDatabase(username=Sys.getenv("DB_USERNAME"),
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
        
      } 
      else {
        
        saveRDS(isolate(CATdesign()), isolate(values$designFile))
        
        values$subject[[paste0(values$subgroup, "Test")]] <- "end"
        
        values$groupIdx <- values$groupIdx +1
        values$subgroup <- groupsToTest[values$groupIdx]
        values$itemsGroup <- items[items$group==values$subgroup, ]
        
        
        #Set maximum number of items in test (stop criterion)
        if (paste0(isolate(values$subgroup), "maxItemNr") %in% txt$text_type) {
          values$maxItemNr <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "maxItemNr"), "text"])
        } else {
          values$maxItemNr <- nrow(itemsGroup)
        }
        
        if (isolate(values$maxItemNr)>nrow(itemsGroup)) {
          values$maxItemNr <- nrow(itemsGroup)
        }
        
        #Set minimum number of items in test 
        if (paste0(isolate(values$subgroup), "minItemNr") %in% txt$text_type) {
          values$minItemNr <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "minItemNr"), "text"])
        } else {
          values$minItemNr <- 0
        }
        
        if (isolate(values$minItemNr)>isolate(values$maxItemNr) | isolate(values$minItemNr)>nrow(itemsGroup)) {
          stop("Minimum item number greater than maximum or available number of items")
        }
        
        #Set se_theta value 
        if (paste0(isolate(values$subgroup), "MirtSeTheta") %in% txt$text_type) {
          values$seTheta <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "MirtSeTheta"), "text"])
        } else {
          values$seTheta <- 0
        }
        
        #Prepare mirtCAT design object
        values$designFile <- paste0("designs/", lang, "-", form, "-", idx, "-", isolate(values$subgroup), ".rds")
        
        if (file.exists(values$designFile)){
          CATdesign <- readRDS(isolate(values$designFile))
        } else {
          
          if (is.na(values$subject[[paste0(values$subgroup, "Theta")]])){
            values$subject[[paste0(values$subgroup, "Theta")]] <- chooseTheta(startThetas, subjectGender, subjectAge, values$subgroup)
          }
          
          params <- values$itemsGroup[, c("a1", "d")]
          mirt_object <- generate.mirt_object(params, '2PL')
          CATdesign <- mirtCAT(mo = mirt_object, 
                               method = txt[txt$text_type == paste0(values$subgroup, "MirtMethod"), "text"], 
                               criteria = txt[txt$text_type == paste0(values$subgroup, "MirtCriteria"), "text"], 
                               start_item = txt[txt$text_type == paste0(values$subgroup, "MirtCriteria"), "text"],
                               design_elements = TRUE, 
                               design = list(thetas.start = values$subject[[paste0(values$subgroup, "Theta")]]))
        }
        
        values$nextItem <- findNextItem(CATdesign)
        
        btnID = paste0(isolate(values$subgroup), "Btn")  
        if (btnID %in% txt$text_type) {
          output$main <- renderUI({h5(txt[txt$text_type == btnID, "text"])})
          output$sidebar <- renderUI({
            actionButton(btnID, label = txt[txt$text_type == "continueBtn", "text"], class = "btn-primary")
          })
          
          header <- paste0(isolate(values$subgroup), "Header")
          headerColor <- paste0(isolate(values$subgroup), "HeaderColor")
          observeEvent(input[[btnID]], {
            output$main <- renderUI({
              list(
                if (header %in% txt$text_type & headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"], style=paste0("color: ", txt[txt$text_type == headerColor, "text"], ";")),
                if (header %in% txt$text_type & !headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"]),
                radioButtons(
                  "question",
                  label = paste0(values$itemsGroup$question[values$nextItem], ' "', values$itemsGroup$item[values$nextItem], '"?'),
                  selected = character(0),
                  choiceNames = strsplit(txt[txt$text_type == "choiceNames", "text"], ",")[[1]],
                  choiceValues = c(0,1)
                )
              )
            })
            
            output$sidebar <- renderUI({div(class = "help-block", txt[txt$text_type == "testInstr", "text"])})
          })
          
        } else {
          
          updateRadioButtons(session, "question",
                             label = paste0(values$itemsGroup$question[values$nextItem], ' "', values$itemsGroup$item[values$nextItem], '"?'),
                             selected = character(0))
        }
        
        values$subject[[paste0(values$subgroup, "Start")]] <- as.character(Sys.time())
        CATdesign(CATdesign)
      }
      
      
      

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