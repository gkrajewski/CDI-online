server <- function(input, output, session) {
  
  #Get available languages
  availableLangs <- list.files(path = LANGUAGES_PATH, recursive = FALSE)
  
  waitingForClose <- reactiveVal(FALSE)
  inventoryStarted <- reactiveVal(FALSE)
  
  observe({
      
    #Read parameters values from URL
    idx <- readFromURL("id", session, caseInvariance = FALSE)
    form <- readFromURL("form", session)
    lang <- readFromURL("lang", session)
    run <- readFromURL("run", session)
    if(is.null(run)){
      run <- "0"
    }
      
    if (!is.null(lang) & !is.null(form) & !is.null(idx)){
      
      if (is.element(lang, availableLangs)){

        #Get available forms for given language
        langPath <- paste0(LANGUAGES_PATH, "/", lang)
        availableForms <- list.files(path = paste0(langPath, "/forms"), recursive = FALSE)
        
        #Get language universal translations
        setwd(langPath)
        uniTransl <- read.csv("uniTranslations.csv", encoding = "UTF-8", sep = ";", strip.white = T)
        setwd(INIT_PATH)
        
        if (is.element(form, availableForms)){
          
          #Specify path to form and set inventory string
          formPath <- paste0(langPath, "/forms/", form)
          urlString <- paste(lang, form, idx, run, sep = "-")
          
          if (!is.element(urlString, BUSY_URLS())){
            
            ### START INVENTORY ###
            inventoryStarted(TRUE)
            
            #Prevent from opening same url params more than once in the same moment
            busyURLs <- BUSY_URLS()
            busyURLs <- c(busyURLs, urlString)
            BUSY_URLS(busyURLs)
            
            session$onSessionEnded(function() {
              busyURLs <- isolate(BUSY_URLS())
              busyURLs <- busyURLs[busyURLs != urlString]
              BUSY_URLS(busyURLs)
            })
            
            closeSession <- reactive({paste0(is.element(urlString, URLS_TO_CLOSE()))})
            observeEvent(closeSession(), {
              if (closeSession()){
                urlsToClose <- URLS_TO_CLOSE()
                urlsToClose <- urlsToClose[urlsToClose != urlString]
                URLS_TO_CLOSE(urlsToClose)
                session$close()
              }
            }, ignoreInit = TRUE)
            
            #Check if user is connected with StarWords app
            if (nchar(idx) == 21){
              fromSW <- TRUE
            } else {
              fromSW <- FALSE
            }
            
            #Prepare items, translations, settings and norms
            setwd(formPath)
            items <- read.csv("items.csv", encoding = "UTF-8", sep = ";", strip.white = T)[c("item_id", "definition", "type", "category")]
            transl <- read.csv("translations.csv", encoding = "UTF-8", sep = ";", strip.white = T) 
            formSettings <- read.csv("settings.csv", encoding = "UTF-8", strip.white = T)
            translID <- paste(transl$text_type, transl$item_type, transl$category)
            uniTranslID <- paste(uniTransl$text_type, uniTransl$item_type, uniTransl$category)
            uniTransl <- subset(uniTransl, !(uniTranslID %in% translID)) #Get things from uniTransl that are not in translations
            setwd(INIT_PATH)
            txt <- rbind(uniTransl, transl)
            settings <- rbind(formSettings, END_SETTINGS)
            typeUniqueSettings <- settings[settings$category == "" | is.na(settings$category), ]
            
            #Modify items: bind some types into one and create categories from old types
            types <- typeUniqueSettings$type
            for (type in types){
              bindedTypesStr <- typeUniqueSettings[typeUniqueSettings$type == type, "binded_types"]
              if (!is.na(bindedTypesStr)){
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
            }
            
            #Prepare user progress
            progressFile <- paste0("usersProgress/", urlString, ".csv")
            if (file.exists(progressFile)){
              userProgress <- read.csv(progressFile, encoding = "UTF-8")
            } else {
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
                disabled = typeUniqueSettings$initially_disabled,
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
            
            logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run, " fromSW=", fromSW))
            
            #Prepare menu buttons (as many as types, except postEnd and postEndSW type)
            menuButtons <- c(1:(length(setdiff(unique(types), c("postEnd", "postEndSW")))))
            menuButtons <- lapply(menuButtons, function(i) {
              
              #Select type for given button
              type <- types[i]
              
              #Adjust button design
              class <- "menuButtonContainer"
              if (userProgress[userProgress$type == type, "done"]) class <- paste(class, "menuButtonContainerDone")
              if (userProgress[userProgress$type == type, "current"]) class <- paste(class, "menuButtonContainerActive")
              title <- ""
              if (is.element(paste0(type, "Tooltip"), txt$text_type)) title <- txt[txt$text_type == paste0(type, "Tooltip"), "text"]
              buttonDiv <- div(title = title, id = paste0(type, "container"), class = class, actionButton(type, label = paste0(i, ". ", txt[txt$text_type == paste0(type,"Btn"), "text"]), class = "btn-primary"))
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
            output$menu <- renderUI({list(menuButtons, disconnectMessage(text = txt[txt$text_type == "error", "text"], refresh = txt[txt$text_type == "refresh", "text"]))})
            
            #Render current type
            reactList <- reactiveVal(list(userProgress = userProgress, answers = answers))
            staticList <- list(types = types, items = items, txt = txt, settings = settings, lang = lang)
            reactList(renderType(input, output, types[match(TRUE, reactList()$userProgress$current)], reactList(), staticList))
            
            #Change category to next when nextBtn clicked
            observeEvent(input$nextBtn, {
              if (reactList()$categoryNr < reactList()$categoriesNr) reactList(renderCategory(input, output, reactList()$categories[reactList()$categoryNr + 1], reactList(), staticList))
            })
            
            #Change category to previous when backBtn clicked
            observeEvent(input$backBtn, {
              if (reactList()$categoryNr != 1) reactList(renderCategory(input, output, reactList()$categories[reactList()$categoryNr - 1], reactList(), staticList))
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
                    sentencesList[[i]] <- input[[paste0("s", i)]]
                  }
                  catAnswer <- paste(sentencesList, collapse = "#")
                } else if (inputType() == "oneCheckboxGroup"){
                  catAnswer <- paste(input$oneCheckboxGroup, collapse =  " ")
                } else if (inputType() == "radio" | inputType() == "manyCheckboxGroups" | inputType() == "radioAlt" | inputType() == "checkboxAlt"){
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
                  catAnswer <- paste(input$birthDate, input$gender, input$filler, input$fillerTxt, sep = "#")
                }
                catAnswer
              } else {
                NULL
              }
            })
            
            observeEvent(catAnswer(),{
              if (!is.null(catAnswer())){
                reactList <- reactList()
                reactList$answers[reactList$answers$type == reactList$type & reactList$answers$category == reactList$category & reactList$answers$answer_type == reactList$settings$input_type, "answer"] <- catAnswer()
                reactList(reactList)
              }
            }, ignoreInit = TRUE)
            
            observeEvent(input$comment, {
              reactList <- reactList()
              reactList$answers[reactList$answers$type == reactList$type & reactList$answers$category == reactList$category & reactList$answers$answer_type == "comment", "answer"] <- input$comment
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
                  if (fromSW) recurrentCallSW(idx, form, lang)
                  reactList$userProgress[reactList$userProgress$type == 'start', "disabled"] <- TRUE
                  disable('start')
                }
                
                #Mark curent type as done
                reactList$userProgress[reactList$userProgress$type == reactList$type, "done"] <- TRUE
                addClass(paste0(reactList$type, "container"), "menuButtonContainerDone")
                
                #Enable/disable some types
                conditionedTypes <- typeUniqueSettings[typeUniqueSettings$type == reactList$type, "conditions"]
                if (is.na(conditionedTypes)) conditionedTypes <- ""
                if (conditionedTypes != ""){
                  conditionedTypes <- strsplit(conditionedTypes, ",")[[1]]
                  conditionedAnswer <- reactList$answers[reactList$answers$type == reactList$type, "answer"]
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
                allEnabledDone <- TRUE
                for (type in types){
                  if (!reactList$userProgress[reactList$userProgress$type == type, "done"] & !reactList$userProgress[reactList$userProgress$type == type, "disabled"]){
                    reactList(renderType(input, output, type, reactList, staticList))
                    allEnabledDone <- FALSE
                    break
                  }
                }
                
                #Render end or postend type
                if(allEnabledDone){
                  
                  logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run, " form completed. Saving..."))
                  if (!reactList$userProgress[reactList$userProgress$type == "end", "done"]){
                    #End type
                    reactList$userProgress[reactList$userProgress$type == "end", "disabled"] <- FALSE
                    enable("end")
                    addClass("end", "endEnabled")
                    reactList(renderType(input, output, "end", reactList, staticList))
                  } else {
                    #Postend type
                    if (fromSW){
                      postEnd <- "postEndSW"
                    } else {
                      postEnd <- "postEnd"
                    }
                    if (!reactList$userProgress[reactList$userProgress$type == type, "done"]){
                      for (type in types){
                        disable(type)
                        reactList$userProgress[reactList$userProgress$type == type, "disabled"] <- TRUE
                      }
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
                      write.csv(reactList$answers, answersFile, row.names = F)
                      logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run, " csv file with asnwers saved"))
                      
                      endDate <- Sys.time()
                      tableName <- paste0("form_", form, "_", lang)
                      answers <- prepareOutput(reactList$answers, idx, lang, form, run, endDate, STRING_LIMIT)
                      
                      dbConnection <- tryCatch( 
                        expr = {
                          storiesDb <- dbConnect(RMariaDB::MariaDB(), user=Sys.getenv("DB_USERNAME"), password=Sys.getenv("DB_PASSWORD"), dbname=Sys.getenv("DB_NAME"), 
                                                 host=Sys.getenv("DB_HOST"), port=Sys.getenv("DB_PORT"))
                          logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run, 
                                          " connected with database. Tables: ", paste(dbListTables(storiesDb), collapse=" "), " tableName=", tableName))
                        
                          if (!(tableName %in% dbListTables(storiesDb))) {
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
                            `answer1` VARCHAR(", toString(STRING_LIMIT), ") CHARACTER SET utf8 COLLATE utf8_general_ci NULL,
                            `answer2` VARCHAR(100) NULL);")
                            rsInsert <- dbSendQuery(storiesDb, query)
                            dbClearResult(rsInsert)
                          }
                          
                          dbWriteTable(storiesDb, value = answers, row.names = FALSE, name = tableName, append = TRUE )
                          dbDisconnect(storiesDb)
                          logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run, " saved in database"))
                          
                          },
                        error = function(e) {
                          logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run, " ", e))
                        }
                      )
                     
                      emailSender <- tryCatch( 
 
                        expr = {
                          
                          email <- envelope() %>%
                            from(MAIL_USERNAME) %>%
                            to(EMAILS_RECIPIENTS) %>%
                            subject(paste0("[SHINYDATA] ", urlString)) %>%
                            text("Inventory completed.") %>%
                            attachment(c(answersFile))
                          
                          smtp <- emayili::server(host = "smtp.gmail.com",
                                         port = 465,
                                         username = MAIL_USERNAME,
                                         password = Sys.getenv("GMAIL_PASSWORD"))
                          
                          smtp(email, verbose = TRUE)

                          logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run, " email sent"))
                        },
                        error = function(e) {
                          logerror(paste0("id=", idx, " form=", form, " lang=", lang, " run=", run))
                          logerror(e)
                        }
                      )
                      
                    }
                    reactList(renderType(input, output, postEnd, reactList, staticList))
                  }
                }   
                
              }#end canConfirm
              
            })
            
            #Save data to file when session ends
            session$onSessionEnded(function() {
              write.csv(isolate(reactList()$answers), answersFile, row.names = F, fileEncoding = "UTF-8")
              write.csv(isolate(reactList()$userProgress), progressFile, row.names = F, fileEncoding = "UTF-8")
            })
            
          } else if (!waitingForClose() & !inventoryStarted()){
            urlsToClose <- URLS_TO_CLOSE()
            urlsToClose <- c(urlsToClose, urlString)
            URLS_TO_CLOSE(urlsToClose)
            waitingForClose(TRUE)
          }
          
        } else {
          output$sidebar <- renderText({paste0(c(uniTransl[uniTransl$text_type == "badForm", "text"], availableForms), collapse = " ")})
        }
        
      } else {
        output$sidebar <- renderText({paste0(c("Bad value of lang parameter in URL. Accesible values are: ", availableLangs), collapse = " ")})
      }
      
    } else {
      # updateQueryString(paste0("?id=", "test", "&form=", "wg", "&lang=", "pl")) #/?id=IlYaL6gzKieyRx92YUl1a&form=wg&lang=pl
      # session$reload()
      output$sidebar <- renderText({"No needed params in URL (lang, form and id)"})
    }  
    
  })
  
}