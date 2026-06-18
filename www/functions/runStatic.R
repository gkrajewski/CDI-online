runStatic <- function(input, output, session, lang, form, idx, run, urlString, fromSW){
  
  #Read and prepare items, translations and settings
  inputFilesRead <- tryCatch(
    expr = {
      
      #Load files
      typePath <- paste0(LANGUAGES_PATH, "/", lang, "/forms/static")
      formPath <- paste0(typePath, "/", form)
      
      uniTransl <- readInputFile(path = typePath, fileName = "uniTranslations.csv")
      items <- readInputFile(path = formPath, fileName = "items.csv")
      transl <- readInputFile(path = formPath, fileName = "translations.csv")
      settings <- readInputFile(path = formPath, fileName = "settings.csv", sep=",")
      parameters <- readInputFile(path = formPath, fileName = "parameters.csv")
      
      #Prepare data frames
      translID <- paste(transl$text_type, transl$item_type, transl$category)
      uniTranslID <- paste(uniTransl$text_type, uniTransl$item_type, uniTransl$category)
      uniTransl <- subset(uniTransl, !(uniTranslID %in% translID)) #Get things from uniTransl that are not in translations
      txt <- rbind(uniTransl, transl)
      typeUniqueSettings <- settings[settings$category == "", ]
      
      #Modify items: bind some types into new one and treat old types as categories
      types <- typeUniqueSettings$type
      for (type in types){
        bindedTypesStr <- typeUniqueSettings[typeUniqueSettings$type == type, "binded_types"]
        if (bindedTypesStr == "startWith"){
          items[substr(items$type, 1, nchar(type)) == type, "category"] <- items[substr(items$type, 1, nchar(type)) == type, "type"]
          items[substr(items$type, 1, nchar(type)) == type, "type"] <- type
        } else {
          bindedTypes <- strsplit(typeUniqueSettings[typeUniqueSettings$type == type, "binded_types"], ",")[[1]]
          for (bindedType in bindedTypes){
            items[substr(items$type, 1, nchar(bindedType)) == bindedType, "category"] <- items[substr(items$type, 1, nchar(bindedType)) == bindedType, "type"]
            items[substr(items$type, 1, nchar(bindedType)) == bindedType, "type"] <- type
          }
        }
      }
      
      TRUE
    },
    error = function(m){
      msg <- paste0("There is problem with input files <br><br>", m)
      logerror(msg)
      output$sidebar <- renderText({msg})
      return(FALSE)
    },
    warning = function(m){
      msg <- paste0("There is problem with input files <br><br>", m)
      logwarn(msg)
      output$sidebar <- renderText({msg})
      return(FALSE)
    }
  )

  if (inputFilesRead){
    
    #Render nice message when error
    output$dcMessage <- renderUI({disconnectMessage(text = paste0(txt[txt$text_type == "error", "text"], " [", urlString, "]"), refresh = txt[txt$text_type == "refresh", "text"])})
    
    #Prepare user progress
    progressFile <- paste0("usersProgress/", urlString, ".csv")
    if (file.exists(progressFile)){
      userProgress <- read.csv(progressFile, encoding = "UTF-8")
    } else {
      if (fromSW) recurrentCallSW(idx, form, lang) #SW call - first contact with inventory
      firstCats <- c()
      i <- 1
      for (type in types){
        uniqueCategories <- unique(txt[txt$item_type == type, "category"])
        categoriesNum <- length(uniqueCategories)
        firstCat <- uniqueCategories[1]
        if (categoriesNum > 1 & uniqueCategories[1] == "") firstCat <- uniqueCategories[2]
        if (firstCat == "allInput") firstCat <- uniqueCategories[3]
        firstCats[i] <- firstCat
        i <- i + 1
      }
      userProgress <- data.frame(
        type = types,
        done = FALSE,
        disabled = as.logical(typeUniqueSettings$initially_disabled),
        current = c(TRUE, rep(FALSE, length(types) - 1)), #Make 1st type as current
        category = firstCats
      )
    }
    
    #Prepare answers
    answersFile <- paste0("answers/", urlString, ".csv")
    if (file.exists(answersFile)){
      answers <- read.csv(answersFile, encoding = "UTF-8")
    } else {
      startDate <- Sys.time()
      answers <- data.frame(type = "none", category = "none", answer_type = "none", answer = as.character(startDate))
    }
    
    #Prepare menu buttons (as many as types, except postEnd and postEndSW type)
    menuButtons <- c(1:(length(setdiff(unique(types), c("postEnd", "postEndSW")))))
    menuButtons <- lapply(menuButtons, function(i) {
      
      #Select type for given button
      type <- types[i]
      
      #Adjust button design
      class <- "menuButtonContainer"
      if (userProgress[userProgress$type == type, "done"]) class <- paste(class, "menuButtonContainerDone")
      if (userProgress[userProgress$type == type, "current"]) class <- paste(class, "menuButtonContainerActive")
      
      buttonDiv <- div(
        title = txt[txt$item_type == type & txt$text_type == "tooltip", "text"],
        id = paste0(type, "container"),
        class = class,
        actionButton(type, label = paste0(i, ". ", txt[txt$item_type == type & txt$text_type == "menuButton", "text"]), class = "btn-primary")
        )
      
      if (userProgress[userProgress$type == type, "disabled"]) buttonDiv <- disabled(buttonDiv)
      
      #Add button observer
      observeEvent(input[[type]], {
        reactList(renderType(input, output, type, reactList(), staticList))
      })
      
      #Return prepared div with button
      return(buttonDiv)
      
    })
    
    #Render type-universal UI
    output$cdiNamePrefix <- renderText({txt[txt$text_type == "cdiNamePrefix", "text"]})
    output$cdiNameSufix <- renderText({txt[txt$text_type == "cdiNameSufix", "text"]})
    output$menu <- renderUI({menuButtons})
    
    #Render current type
    reactList <- reactiveVal(list(userProgress = userProgress, answers = answers))
    staticList <- list(types = types, items = items, txt = txt, settings = settings, parameters = parameters, 
                       lang = lang, idx=idx, urlString = urlString, fromSW = fromSW)
    reactList(renderType(input, output, types[match(TRUE, reactList()$userProgress$current)], reactList(), staticList))
    
    #Change category to next when nextBtn clicked
    observeEvent(input$nextBtn, {
      disable("nextBtn")
      disable("backBtn")
      if (reactList()$categoryNr < reactList()$categoriesNr) {
        reactList(renderCategory(input, output, reactList()$categories[reactList()$categoryNr + 1], reactList(), staticList))
      }
    })
    
    #Change category to previous when backBtn clicked
     observeEvent(input$backBtn, {
       disable("backBtn")
       disable("nextBtn")
       if (reactList()$categoryNr != 1) {
         reactList(renderCategory(input, output, reactList()$categories[reactList()$categoryNr - 1], reactList(), staticList))
       }
     })
    
    #Add input saving
    inputType <- reactive({
      if (reactList()$inputPage){
        reactList()$settings$input_type
      } else {
        NULL
      }
    })
    
    sentencesNr <- reactive({
      reactList()$sentencesNr
    })
    
    currItems <- reactive({
      reactList()$items
    })
    
    catAnswer <- reactive({
      if (!is.null(inputType())){
        catAnswer <- ""
        if (inputType() == "sentences"){
          sentencesList <- list()
          for (i in 1:sentencesNr()){
            sentencesList[[i]] <- gsub('[#]', '', input[[paste0("s", i)]])
          }
          catAnswer <- paste(sentencesList, collapse = "#")
        } else if (inputType() == "oneCheckboxGroup"){
          catAnswer <- paste(input$oneCheckboxGroup, collapse =  " ")
        } else if (inputType() == "radio" | inputType() == "radioVertical" | inputType() == "manyCheckboxGroups" | inputType() == "radioAlt" | inputType() == "checkboxAlt"){
          answersPattern <- c()
          for (i in 1:nrow(currItems())){
            id <- paste0("mQ", i)
            if (!is.null(input[[id]])){
              answer <- paste0(input[[id]], collapse = " ")
              answersPattern[[i]] <- answer
            } else {
              answersPattern[[i]] <- "0"
            }
          }
          catAnswer <- paste(answersPattern, collapse = ",")
        } else if (inputType() == "demographic"){
          catAnswer <- paste(input$birthDate, input$gender, gsub('[#]', '', input$filler), gsub('[#]', '', input$fillerTxt), sep = "#")
        }
        catAnswer
      } else {
        NULL
      }
    })
    
    observeEvent(catAnswer(),{
      if (!is.null(catAnswer())){
        reactList <- reactList()
        catAns = catAnswer()

        if (inputType() == "oneCheckboxGroup") {
          choiceValues <- replicate(nrow(currItems()), 0)
          chosen <- unlist(strsplit(catAnswer(), " "))

          if(length(chosen) > 0){
            for (i in chosen) {
              choiceValues[as.numeric(i)] = 1
            }
          }

          catAns <- paste0(choiceValues, collapse = " ")
        }

        reactList$answers[reactList$answers$type == reactList$type & reactList$answers$category == reactList$category & reactList$answers$answer_type == reactList$settings$input_type, "answer"] <- catAns
        reactList(reactList)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$comment, {
      reactList <- reactList()
      comment_tmp = input$comment
      comment_tmp <- gsub("\n", " ", comment_tmp)
      comment_tmp <- trimws(comment_tmp)
      if (comment_tmp == '') {
        comment_tmp = NA
      }
      
      reactList$answers[reactList$answers$type == reactList$type & reactList$answers$category == reactList$category & reactList$answers$answer_type == "comment", "answer"] <- comment_tmp
      reactList(reactList)
    })
    
    #Confirm type when confBtn clicked
    observeEvent(input$confBtn, {
      
      reactList <- reactList()
      canConfirm <- TRUE
      
      #Check if can be confirmed if type=start
      if (reactList$type == "start"){
        if (is.null(input$gender)){
          canConfirm <- FALSE
          showModal(modalDialog(
            title = txt[txt$text_type == "modalTitle", "text"],
            txt[txt$text_type == "noGender", "text"],
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          norms <- readNorms(formPath, form) #Could be gender dependent thus it is here
          if (!is.null(norms)){
            childAge <- interval(input$birthDate, Sys.Date()) %/% months(1)
            monthsInNorms <- lapply(row.names(norms), function(i){as.numeric(substr(i, 3, nchar(i)))})
            if (!is.element(childAge, monthsInNorms)){
              canConfirm <- FALSE
              showModal(modalDialog(
                title = txt[txt$text_type == "modalTitle", "text"],
                txt[txt$text_type == "badDate", "text"],
                easyClose = TRUE,
                footer = NULL
              ))
            } else if (is.null(input$filler)){
              canConfirm <- FALSE
              showModal(modalDialog(
                title = txt[txt$text_type == "modalTitle", "text"],
                txt[txt$text_type == "noFiller", "text"],
                easyClose = TRUE,
                footer = NULL
              ))
            }
          } else if (is.null(input$filler)){
            canConfirm <- FALSE
            showModal(modalDialog(
              title = txt[txt$text_type == "modalTitle", "text"],
              txt[txt$text_type == "noFiller", "text"],
              easyClose = TRUE,
              footer = NULL
            ))
          }
        }
      }

      #Confirm type
      if (canConfirm){
        
        if (reactList$type == "start"){
          #Disable start btn
          reactList$userProgress[reactList$userProgress$type == 'start', "disabled"] <- TRUE
          disable('start')
        }
        
        #Log info about done type
        loginfo(paste0(urlString, " part '", reactList$type, "' done"))

        #Mark curent type as done
        reactList$userProgress[reactList$userProgress$type == reactList$type, "done"] <- TRUE
        addClass(paste0(reactList$type, "container"), "menuButtonContainerDone")
        
        #Enable/disable some types
        conditionedTypes <- typeUniqueSettings[typeUniqueSettings$type == reactList$type, "conditions"]
        if (conditionedTypes != ""){
          conditionedTypes <- strsplit(conditionedTypes, ",")[[1]]
          conditionedAnswer <- reactList$answers[reactList$answers$type == reactList$type & reactList$answers$answer_type == settings[settings$type == reactList$type, 'input_type'], "answer"]
          possibleAnswers <- typeUniqueSettings[typeUniqueSettings$type == reactList$type, "answers_to_enable"]
          possibleAnswers <- strsplit(possibleAnswers, ";")[[1]]
          for (i in 1:length(conditionedTypes)){
            conditionedType <- conditionedTypes[i]
            if (length(possibleAnswers) >= i){
              possibleAnswer <- possibleAnswers[i]
            } else {
              possibleAnswer <- possibleAnswers[length(possibleAnswers)]
            }
            if (possibleAnswer != "all") possibleAnswer <- strsplit(possibleAnswer, ",")[[1]]
            if (is.element(conditionedAnswer, possibleAnswer) | possibleAnswer == "all"){
              reactList$userProgress[reactList$userProgress$type == conditionedType, "disabled"] <- FALSE
              enable(conditionedType)
            } else {
              reactList$userProgress[reactList$userProgress$type == conditionedType, "disabled"] <- TRUE
              disable(conditionedType)
              removeClass(paste0(conditionedType, "container"), "menuButtonContainerDone")
            }
          }
        }

        #Render 1st not done type (from left)
        reactList$allEnabledDone <- TRUE
        for (type in types){
          if (!reactList$userProgress[reactList$userProgress$type == type, "done"] & !reactList$userProgress[reactList$userProgress$type == type, "disabled"]){
            reactList(renderType(input, output, type, reactList, staticList))
            reactList$allEnabledDone <- FALSE
            break
          }
        }

        #Render end type or postend message
        if(reactList$allEnabledDone){
          
          if (!reactList$userProgress[reactList$userProgress$type == "end", "done"]){
            
            #Render end type
            reactList$userProgress[reactList$userProgress$type == "end", "disabled"] <- FALSE
            enable("end")
            addClass("end", "endEnabled")
            reactList(renderType(input, output, "end", reactList, staticList))
            
          } else {

            if (!reactList$userProgress[reactList$userProgress$type == type, "disabled"]){
              
              ### INVENTORY END ###
              loginfo(paste0(urlString, " form completed. Saving..."))
              
              #Show info about end
              if (fromSW) endMsg <- "endMsgTextSW" else endMsg <- "endMsgText"
              endMsgtxt <- reactList$txt[reactList$txt$text_type == endMsg, "text"]
              
              additionalMessage <- staticList$parameters[staticList$parameters$parameter=="additionalEndMessageFromDatabase", "value"]
              if (length(additionalMessage)>0 && additionalMessage=="yes") {
                additionalMessageTxt <- getAdditionalEndMessage(urlString, "database", staticList$parameters, staticList$txt)
                endMsgtxt <- paste(endMsgtxt, "<br><br>", additionalMessageTxt)
              }
              
              #Prepare redirection
              if ("redirectionURL" %in% staticList$parameters$parameter){
                footer <- list(
                  actionButton("redirect", reactList$txt[reactList$txt$text_type == "redirectionBtn", "text"], class = "btn-primary"),
                  div(id="redirectionText", reactList$txt[reactList$txt$text_type == "redirectionText", "text"])
                  )
                
                observeEvent(input$redirect, {
                  redirect(staticList$parameters, idx, lang, form, "static", run)
                }, once = TRUE)
                
              } else {
                
                footer <- NULL
                
              }
              
              #Show end message
              showModal(modalDialog(
                title = reactList$txt[reactList$txt$text_type == "endMsgTitle", "text"],
                HTML(endMsgtxt),
                easyClose = FALSE,
                footer = footer
              ))
              
              #Disable redirection button for now (if created)
              if ("redirectionURL" %in% staticList$parameters$parameter) disable("redirect")
              
              #Disable all types
              for (type in types){
                disable(type)
                reactList$userProgress[reactList$userProgress$type == type, "disabled"] <- TRUE
              }
              
              #reactList(renderType(input, output, "end", reactList, staticList))
              
              #Count score and call StarWords database
              if (fromSW){
                
                score <- "false"
                norms <- readNorms(formPath, form)
                if (!is.null(norms)){
                  demoAnswer <- reactList$answers[reactList$answers$answer_type == "demographic", "answer"]
                  demoAnswer <- strsplit(demoAnswer, "#")[[1]]
                  birthDate <- demoAnswer[1]
                  age <- interval(birthDate, Sys.Date()) %/% months(1)
                  if (countScore(reactList$answers, typeUniqueSettings) <= norms[paste0("m_", age), "p_0.1"]) score <- "true"
                }
                recurrentCallSW(idx, form, lang, done = "true", score)
                
              }
              
              #Save csv with answers
              write.csv(reactList$answers, answersFile, row.names = F)
              loginfo(paste0(urlString, " csv file with asnwers saved"))

              #Save answers to database
              if (staticList$parameters[staticList$parameters$parameter == "database", "value"]=="yes") {
                endDate <- Sys.time()
                tableName <- paste0("form_", form, "_", lang)

                answers <- prepareOutputStatic(reactList$answers, idx, lang, form, run, endDate, reactList$typeItems, STRING_LIMIT)

                query = paste0("CREATE TABLE `", Sys.getenv("DB_NAME"), "`.`",tableName,"` (
                            `id` VARCHAR(99) NOT NULL,
                            `lang` VARCHAR(45) NULL,
                            `form` VARCHAR(45) NULL,
                            `run` VARCHAR(45) NULL,
                            `start_date` DATETIME NULL,
                            `end_date` DATETIME NULL,
                            `type` VARCHAR(45) NULL,
                            `category` VARCHAR(45) NULL,
                            `answer_type` VARCHAR(45) NULL,
                            `question_id` INT NULL,
                            `answer_id` VARCHAR(45) NULL,
                            `answer1` VARCHAR(", toString(STRING_LIMIT), ") CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `answer2` VARCHAR(100) NULL);")
                
                sendDatabase(username=Sys.getenv("DB_USERNAME"),
                                             password=Sys.getenv("DB_PASSWORD"),
                                             dbname=Sys.getenv("DB_NAME"),
                                             host=Sys.getenv("DB_HOST"),
                                             port=Sys.getenv("DB_PORT"),
                                             id=urlString,
                                             tableName=tableName,
                                             tableCreate=query,
                                             tableInput=answers)
              }

              #Send e-mail
              if (staticList$parameters[staticList$parameters$parameter == "email", "value"]=="yes") {
                
                loginfo(paste0(urlString, " sending email"))
                sendMail(subject=paste0("[SHINYDATA] ", urlString),
                         body="Inventory completed.",
                         id=urlString,
                         attach=answersFile
                )
                
              } else {
                
                loginfo(paste0(urlString, " sending emails disabled"))
                
              }
              
              #Send logs to database
              sendLogs(urlString, idx, form, lang)
              
              #Enable redirection button (if created)
              if ("redirectionURL" %in% staticList$parameters$parameter) enable("redirect")
              
              reactList(reactList)
            }

          }

        }#end reactList$allEnabledDone

      }#end canConfirm

    })#end observeEvent
    
    #Save data to file when session ends
    session$onSessionEnded(function() {
      
      write.csv(isolate(reactList()$answers), answersFile, row.names = F, fileEncoding = "UTF-8")
      write.csv(isolate(reactList()$userProgress), progressFile, row.names = F, fileEncoding = "UTF-8")

      if (isolate(reactList()$allEnabledDone)) {
        sendLogs(urlString, idx, form, lang)
      }
      
    })
    
  }
  
}