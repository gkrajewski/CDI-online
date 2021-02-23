server <- function(input, output, session) {
  
  observe({
    
    id <- readFromURL("id", session)
    form <- readFromURL("form", session)
    lang <- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(id)){
      
      if (length(id) == 21){
        connected <- callSW(form = form, id = id)
        notFromSW = FALSE
      } else {
        connected = FALSE
        notFromSW = TRUE
      }
      
      if (connected | notFromSW){
        
        #Prepare items, translations, settings and data files
        setwd(paste0(dataPath, "/", lang))
        uniTransl <- read.csv("uniTranslations.csv", encoding = "UTF-8", sep = ";", strip.white = T)
        setwd(paste0(dataPath, "/", lang, "/", form))
        items <<- read.csv("items.csv", encoding = "UTF-8", sep = ";", strip.white = T)[1:6]
        #TODO: norms <<- read.csv("comp.csv", encoding = "UTF-8", strip.white = T, row.names = 1)
        transl <- read.csv("translations.csv", encoding = "UTF-8", sep = ";", strip.white = T) 
        if (lang == "no") transl$text.1 <- NULL #TODO
        formSettings <- read.csv("settings.csv", encoding = "UTF-8", strip.white = T)
        setwd(initPath)
        txt <<- rbind(uniTransl, transl)
        settings <<- rbind(formSettings, endSettings)
        typeUniqueSettings <<- settings[settings$category == "" | is.na(settings$category), ]
        types <<- typeUniqueSettings$type
        typesNr <- length(types)
        userProgressFile <- paste0("usersProgress/", lang, "-", form, "-", id, ".csv")
        answersFile <- paste0("answers/", lang, "-", form, "-", id, ".csv")
        
        #Bind some types and create artificial categories
        for (type in types){
          
          bindedTypesStr <- typeUniqueSettings[typeUniqueSettings$type == type, "binded_types"]
          
          if (!is.na(bindedTypesStr)){
            
            if (bindedTypesStr == "startWith"){
              
              items[substr(items$type, 1, nchar(type)) == type, "category"] <<- items[substr(items$type, 1, nchar(type)) == type, "type"]
              items[substr(items$type, 1, nchar(type)) == type, "type"] <<- type
              
            } else {
              
              bindedTypes <- strsplit(typeUniqueSettings[typeUniqueSettings$type == type, "binded_types"], ",")[[1]]
              
              for (bindedType in bindedTypes){
                
                items[substr(items$type, 1, nchar(bindedType)) == bindedType, "category"] <<- items[substr(items$type, 1, nchar(bindedType)) == bindedType, "type"]
                items[substr(items$type, 1, nchar(bindedType)) == bindedType, "type"] <<- type
                
              }
              
            }
            
          }
          
        }
        
        #Prepare user progress csv
        if (file.exists(userProgressFile)){
          userProgress <<- read.csv(userProgressFile, encoding = "UTF-8")
        } else {
          
          firstCats <- c()
          i <- 1
          
          for (type in types){
            uniqueCategories <- unique(txt[txt$item_type == type, "category"])
            categoriesNum <- length(uniqueCategories)
            firstCat <- uniqueCategories[1]
            if (categoriesNum > 1 & uniqueCategories[1] == "") firstCat <- uniqueCategories[2]
            firstCats[i] <- firstCat
            i <- i + 1
          }
          
          userProgress <<- data.frame(
            type = types,
            done = FALSE,
            disabled = typeUniqueSettings$initially_disabled,
            current = c(TRUE, rep(FALSE, typesNr - 1)), #Make 1st type as current
            category = firstCats
          )
          
        }
        
        #Prepare answers csv
        if (file.exists(answersFile)){
          answers <<- read.csv(answersFile, encoding = "UTF-8")
        } else {
          answers <<- data.frame(type = "none", category = "none", answer_type = "none", answer = "none")
        }
        
        #Prepare type buttons
        typeButtonsDivs <<- list()
        lapply(1:(typesNr - 1), function(i) {
          
          type <- types[i]
          class <- "menuButtonContainer"
          if (userProgress[userProgress$type == type, "done"]) class <- paste(class, "menuButtonContainerDone")
          if (userProgress[userProgress$type == type, "current"]) class <- paste(class, "menuButtonContainerActive")
          title <- ""
          if (is.element(paste0(type, "Tooltip"), txt$text_type)) title <- txt[txt$text_type == paste0(type, "Tooltip"), "text"]
          buttonDiv <- div(title = title, id = paste0(type, "container"), class = class, actionButton(type, label = paste0(i, ". ", txt[txt$text_type == paste0(type,"Btn"), "text"]), class = "btn-primary"))
          if (userProgress[userProgress$type == type, "disabled"]) buttonDiv <- disabled(buttonDiv)
          typeButtonsDivs[[i]] <<- buttonDiv
          
          observeEvent(input[[type]], {
            renderType(input, output, type)
          })
          
        })
        
        #Render universal UI
        output$cdiNamePrefix <- renderText({txt[txt$text_type == "cdiNamePrefix", "text"]})
        output$cdiNameSufix <- renderText({txt[txt$text_type == "cdiNameSufix", "text"]})
        output$menu <- renderUI({typeButtonsDivs})
        
        #Render first type
        type <- types[match(TRUE, userProgress$current)]
        language <<- lang #needed for dateInput
        renderType(input, output, type)
        addSidebarObservers(input, output, form)
        addDataSaving(input)

        #Save data
        session$onSessionEnded(function() {
          # write.csv(answers, answersFile, row.names = F)
          # write.csv(userProgress, userProgressFile, row.names = F)
        })
        
      } else {
        
        output$sidebar <- renderUI({"ERROR: Cannot connect with StarWords app"})
        
      }
      
    } else {
      
      #No URL parameters
      updateQueryString(paste0("?id=", "test", "&form=", "wg", "&lang=", "pl"))
      session$reload()

    }
    
  })#end observe
  
}#end server