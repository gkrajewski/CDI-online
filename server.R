server <- function(input, output, session) {
  
  #Get available languages
  availableLangs <- list.files(path = LANGUAGES_PATH, recursive = FALSE)
  
  #Set variables used for preventing opening the same inventory more than once
  waitingForClose <- reactiveVal(FALSE)
  inventoryStarted <- reactiveVal(FALSE)
  
  observe({
      
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

        #Get available forms for given language
        langPath <- paste0(LANGUAGES_PATH, "/", lang)
        availableStaticForms <- list.files(path = paste0(langPath, "/forms/static"), recursive = FALSE)
        availableAdaptiveForms <- list.files(path = paste0(langPath, "/forms/adaptive"), recursive = FALSE)
        
        #Get language universal translations
        setwd(langPath)
        settings <- read.csv("preSettings.csv", encoding = "UTF-8", sep = ";", strip.white = T)
        setwd(INIT_PATH)
        
        if (type=="static" | type=="adaptive") {
          
          if (type=="static") {
            availableForms = availableStaticForms
          } else {
            availableForms = availableAdaptiveForms
          }
          
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
                
              if (type=="adaptive") {
                runAdaptive(input, output, session, lang, form, idx, run, urlString, fromSW)
              } else {
                runStatic(input, output, session, lang, form, idx, run, urlString, fromSW)
              }
                
            } else if (!waitingForClose() & !inventoryStarted()){
              urlsToClose <- URLS_TO_CLOSE()
              urlsToClose <- c(urlsToClose, urlString)
              URLS_TO_CLOSE(urlsToClose)
              waitingForClose(TRUE)
            }
            
          } else {
            
            #Bad form
            output$sidebar <- renderText({paste0(c(settings[settings$text_type == "badForm", "text"], 
                                                   " type=static: ",
                                                   availableStaticForms, 
                                                   ", type=adaptive: ",
                                                   availableAdaptiveForms,
                                                   "<br><br>", 
                                                   settings[settings$text_type == "errorInfo", "text"],
                                                   "<br><br>link:",
                                                   getWholeURL(session)), collapse = " ")})
            
          }
          
        } else {
          
          #Bad type
          output$sidebar <- renderText({paste0(c(settings[settings$text_type == "badType", "text"], 
                                                 "static, adaptive",
                                                 "<br><br>", 
                                                 settings[settings$text_type == "errorInfo", "text"],
                                                 "<br><br>link:",
                                                 getWholeURL(session)), collapse = " ")})
        }
        
      } else {
        
        #Bad language
        output$sidebar <- renderText({paste0(c("Bad value of lang parameter in URL. Accesible values are: ", 
                                               availableLangs,
                                               "<br><br>link:",
                                               getWholeURL(session)), collapse = " ")})
      }
      
    } else {
      
      #No all needed parameters
      output$sidebar <- renderText({paste0("No needed params in URL (lang, form and id) <br><br>link: ", 
                                           getWholeURL(session))})
      
      # Useful for testing
      # updateQueryString(paste0("?id=", "test", "&form=", "ws", "&lang=", "pl")) #/?id=IlYaL6gzKieyRx92YUl1a&form=wg&lang=pl
      # session$reload()
      
    }  
    
  })#end observe
  
}#end server