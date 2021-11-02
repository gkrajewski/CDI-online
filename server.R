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
        availableStaticForms <- list.files(path = paste0(langPath, "/forms/static"), recursive = FALSE)
        availableAdaptiveForms <- list.files(path = paste0(langPath, "/forms/adaptive"), recursive = FALSE)
        
        #Get language universal translations
        setwd(langPath)
        settings <- read.csv("preSettings.csv", encoding = "UTF-8", sep = ";", strip.white = T)
        setwd(INIT_PATH)
        
        if (is.element(form, c(availableStaticForms, availableAdaptiveForms))){
          
          #Set inventory string
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
            
            if (endsWith(form, "-cat")) {
              runAdaptive(input, output, session, lang, form, idx, run)
            } else {
              runStatic(input, output, session, lang, form, idx, run)
            }

          } else if (!waitingForClose() & !inventoryStarted()){
            urlsToClose <- URLS_TO_CLOSE()
            urlsToClose <- c(urlsToClose, urlString)
            URLS_TO_CLOSE(urlsToClose)
            waitingForClose(TRUE)
          }
          
        } else {
          url = paste0(session$clientData$url_protocol,"//", 
                       session$clientData$url_hostname,":",
                       session$clientData$url_port,
                       session$clientData$url_pathname,
                       session$clientData$url_search)
          output$sidebar <- renderText({paste0(c(settings[settings$text_type == "badForm", "text"], 
                                                 " type=static: ",
                                                 availableStaticForms, 
                                                 ", type=adaptive: ",
                                                 availableAdaptiveForms,
                                                 "<br><br>", 
                                                 settings[settings$text_type == "errorInfo", "text"],
                                                 "<br><br>link:",
                                                 url), collapse = " ")})

        }
        
      } else {
        url = paste0(session$clientData$url_protocol,"//", 
                     session$clientData$url_hostname,":",
                     session$clientData$url_port,
                     session$clientData$url_pathname,
                     session$clientData$url_search)
        output$sidebar <- renderText({paste0(c("Bad value of lang parameter in URL. Accesible values are: ", 
                                               availableLangs,
                                               "<br><br>link:",
                                               url), collapse = " ")})
      }
      
    } else {
      # updateQueryString(paste0("?id=", "test", "&form=", "ws", "&lang=", "pl")) #/?id=IlYaL6gzKieyRx92YUl1a&form=wg&lang=pl
      # session$reload()
      url = paste0(session$clientData$url_protocol,"//", 
                   session$clientData$url_hostname,":",
                   session$clientData$url_port,
                   session$clientData$url_pathname,
                   session$clientData$url_search)
      output$sidebar <- renderText({paste0("No needed params in URL (lang, form and id) <br><br>link: ", 
                                           url)})
      
    }  
    
  })
  
}