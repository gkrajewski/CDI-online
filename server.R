server <- function(input, output, session) {
  
  #Get available languages
  availableLangs <- list.files(path = LANGUAGES_PATH, recursive = FALSE)
  
  #Set variables used for preventing opening the same inventory more than once
  waitingForClose <- reactiveVal(FALSE)
  inventoryStarted <- reactiveVal(FALSE)
  
  observe({
    
    Sys.sleep(5)
      
    #Read parameters values from URL
    idx <- readFromURL("id", session, caseInvariance = FALSE)
    form <- readFromURL("form", session)
    lang <- readFromURL("lang", session)
    type <- readFromURL("type", session)
    run <- readFromURL("run", session)
    
    if (is.null(run)){
      run <- "0"
    }
    
    if (is.null(type)){
      type <- "static"
    }
      
    if (!is.null(lang) & !is.null(form) & !is.null(idx)){
      
      if (is.element(lang, availableLangs)){

        #Get language universal translations
        langPath <- paste0(LANGUAGES_PATH, "/", lang)
        setwd(langPath)
        inputFilesRead <- tryCatch(
          expr = {
            settings <- read.csv("preSettings.csv", encoding = "UTF-8", sep = ";", strip.white = T)
          },
          error = function(m){
            msg <- paste0("There is problem with input files <br><br>", m)
            logerror(msg)
            output$sidebar <- renderText({msg})
            return(FALSE)
          }
        )
        setwd(INIT_PATH)

        #Get allowed by app types
        allowedTypes <- c("static", "adaptive")
        
        if (is.element(type, allowedTypes)) {
          
          #Get available types for given language
          availableTypes <- list.files(path = paste0(langPath, "/forms/"), recursive = FALSE)
          
          if (is.element(type, availableTypes)){
            
            #Get available forms for given type
            typePath <- paste0(langPath, "/forms/", type)
            availableForms <- list.files(path = paste0(langPath, "/forms/", type), recursive = FALSE)
            availableForms <- availableForms[!endsWith(availableForms, ".csv")] #get rid of csv file
            
            if (is.element(form, availableForms)){
              
              #Set inventory string
              urlString <- paste(lang, form, idx, run, sep = "-")
              
              if (!is.element(urlString, BUSY_URLS())){
                
                ### START INVENTORY ###
                inventoryStarted(TRUE)
                addHandler(writeToFile, file=paste0(INIT_PATH, "/logs/", urlString, ".log"), level='DEBUG', 
                           formatter=formatter.shiny)
                
                #Check if user is connected with StarWords app
                if (nchar(idx) == 21){
                  fromSW <- TRUE
                } else {
                  fromSW <- FALSE
                }
                
                #Log info about opening inventory
                loginfo(paste0(urlString, " inventory opened, ",  "fromSW=", fromSW))
                
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
                
                if (type == "adaptive") {
                  loginfo(paste0(urlString, " starting adaptive test"))
                  runAdaptive(input, output, session, lang, form, idx, run, urlString, fromSW)
                } else if (type == "static") {
                  loginfo(paste0(urlString, " starting static test"))
                  runStatic(input, output, session, lang, form, idx, run, urlString, fromSW)
                } else {
                  logerror(paste0("Not allowed type - '", type, "'. Type can be 'static' or 'adaptive'. Change name of form(s) type folder"))
                }
                
              } else if (!waitingForClose() & !inventoryStarted()){
                urlsToClose <- URLS_TO_CLOSE()
                urlsToClose <- c(urlsToClose, urlString)
                URLS_TO_CLOSE(urlsToClose)
                waitingForClose(TRUE)
              }
              
            } else {
              
              #Bad form
              output$sidebar <- renderText({paste0(c(settings[settings$text_type == "noForm", "text"], 
                                                     "<br><br>",
                                                     settings[settings$text_type == "errorInfo", "text"],
                                                     "<br><br>link:",
                                                     getWholeURL(session)), collapse = " ")})
              
              logerror(paste0("Bad value of form parameter: ", form, ". Available values are: ", paste0(availableForms, collapse = ", ")))
              
            }
          
          } else {
            
            #No type folder
            output$sidebar <- renderText({paste0(c(settings[settings$text_type == "noType", "text"], 
                                                   "<br><br>", 
                                                   settings[settings$text_type == "errorInfo", "text"],
                                                   "<br><br>link:",
                                                   getWholeURL(session)), collapse = " ")})
            
            logerror(paste0("Bad value of the type parameter: ", form, ". Available values are: ", paste0(availableTypes, collapse = ", ")))
            
          }
          
        } else {
          
          #Not allowed type
          output$sidebar <- renderText({paste0(c(settings[settings$text_type == "badType", "text"], 
                                                 "<br><br>", 
                                                 settings[settings$text_type == "errorInfo", "text"],
                                                 "<br><br>link:",
                                                 getWholeURL(session)), collapse = " ")})
          
          logerror(paste0("Bad value of the type parameter: ", type, ". Allowed values are: ", paste0(allowedTypes, collapse = ", ")))
        }
        
      } else {
        
        #Bad language
        output$sidebar <- renderText({paste0(c("Bad value of the lang parameter in the URL.", 
                                               "<br><br>link:",
                                               getWholeURL(session)), collapse = " ")})
        
        logerror(paste0("Bad value of the lang parameter: ", lang, ". Available values are: ", paste0(availableLangs, collapse = ", ")))
      }
      
    } else {
      
      #No all needed parameters
      output$sidebar <- renderText({paste0("Missing URL parameters. <br><br> link: ", 
                                           getWholeURL(session))})
      output$cdiNameSufix <- renderText({})
      logerror("Missing URL parameters (lang, form and id).")
      
      # Useful for testing
      # updateQueryString(paste0("?id=", "test", "&form=", "wg", "&lang=", "pl")) #/?id=IlYaL6gzKieyRx92YUl1a&form=wg&lang=pl
      # session$reload()
      
    }  
    
  })#end observe
  
}#end server