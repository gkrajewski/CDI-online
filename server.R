server <- function(input, output, session) {
  
  observe({
    
    #Read parameters values from URL
    id <- readFromURL("id", session)
    form <- readFromURL("form", session)
    lang <- readFromURL("lang", session)
    
    #Render UI if all needed URL parameters are given
    if (!is.null(form) & !is.null(lang) & !is.null(id)){
      
      #Set used language (needed for dateInput)
      language <<- lang
      
      #Get language universal translations
      setwd(paste0(dataPath, "/", lang))
      uniTransl <- read.csv("uniTranslations.csv", encoding = "UTF-8", sep = ";", strip.white = T)
      
      #Get form specific items, translations and settings
      setwd(paste0(dataPath, "/", lang, "/", form))
      items <<- read.csv("items.csv", encoding = "UTF-8", sep = ";", strip.white = T)[1:6]
      transl <- read.csv("translations.csv", encoding = "UTF-8", sep = ";", strip.white = T)
      formSettings <- read.csv("settings.csv", encoding = "UTF-8", strip.white = T)
      parts <<- read.csv("parts.csv", encoding = "UTF-8", strip.white = T)
      setwd(initPath)
      
      #Join form specific and universal translations and settings
      txt <<- rbind(uniTransl, transl)
      settings <<- rbind(uniSettings, formSettings) 
      
      #Render CDI name
      output$cdiNamePrefix <- renderText({txt[txt$text_type == "cdiNamePrefix", "text"]})
      output$cdiNameSufix <- renderText({txt[txt$text_type == "cdiNameSufix", "text"]})
      
      #Get types from parts file
      types <<- parts$type
      
      #Bind some types and create artificial categories as specified in parts file
      for (type in types){
        
        
        if (parts[parts$type == type, "binded_types"] != "none"){

          if (parts[parts$type == type, "binded_types"] == "startWith"){
            
            items[substr(items$type, 1, nchar(type)) == type, "category"] <<- items[substr(items$type, 1, nchar(type)) == type, "type"]
            items[substr(items$type, 1, nchar(type)) == type, "type"] <<- type

          } else {

            bindedTypes <- strsplit(parts[parts$type == type, "binded_types"], ",")[[1]]

            for (bindedType in bindedTypes){
              
              items[substr(items$type, 1, nchar(bindedType)) == bindedType, "category"] <<- items[substr(items$type, 1, nchar(bindedType)) == bindedType, "type"]
              items[substr(items$type, 1, nchar(bindedType)) == bindedType, "type"] <<- type

            }

          }

        }

      }
      
      #Get number of types
      typesNr <- length(types)
      
      #Get first categories
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
      
      #Prepare userProgress file
      userProgressFile <- paste0("userProgress/", lang, "-", form, "-", id, ".csv")
      if (file.exists(userProgressFile)){
        
        userProgress <<- read.csv(userProgressFile, encoding = "UTF-8")
        
      } else {
        
        userProgress <<- data.frame(
          type = types,
          done = FALSE,
          disabled = parts$initially_disabled,
          current = c(TRUE, rep(FALSE, typesNr - 1)), #make 1st type as current
          category = firstCats
        )
        
      }
      
      #Prepare answers file
      answersFile <- paste0("answers/", lang, "-", form, "-", id, ".csv")
      
      if (file.exists(answersFile)){
        answers <<- read.csv(answersFile, encoding = "UTF-8")
      } else {
        answers <<- data.frame(type = "none", category = "none", answer_type = "none", answer = "none")
      }
      
      #Prepare list of type buttons divs
      typeButtonsDivs <<- list()
      
      #Fill in list of type buttons divs (except postEnd type)
      lapply(1:(typesNr - 1), function(i) {

        type <- types[i]
        
        #Prepare css class for div with button
        class <- "menuButtonContainer"
        if (userProgress[userProgress$type == type, "done"]) class <- paste(class, "menuButtonContainerDone")
        if (userProgress[userProgress$type == type, "current"]) class <- paste(class, "menuButtonContainerActive")
        
        #Prepare title for div with button
        title <- ""
        if (is.element(paste0(type, "Tooltip"), txt$text_type)) title <- txt[txt$text_type == paste0(type, "Tooltip"), "text"]
        
        #Prepare button div
        buttonDiv <- div(title = title, id = paste0(type, "container"), class = class, actionButton(type, label = paste0(i, ". ", txt[txt$text_type == paste0(type,"Btn"), "text"]), class = "btn-primary"))
        if (userProgress[userProgress$type == type, "disabled"]) buttonDiv <- disabled(buttonDiv)
        
        #Add button div to list  
        typeButtonsDivs[[i]] <<- buttonDiv
        
        #Render proper page when type button clicked
        observeEvent(input[[type]], {
          renderType(input, output, type)
        })

      })
      
      #Render menu with buttons
      output$menu <- renderUI({typeButtonsDivs})
      
      #Start app
      type <- types[match(TRUE, userProgress$current)] #get current type according to userProgress df
      renderType(input, output, type)
      
      #TODO: HTTP start request
      loginToFirebase(output)
      
      #Add observers (sidebar buttons and input objects)
      addSidebarObservers(input, output, form)
      addDataSaving(input, output)
      
      #Save answers and userProgress to csv file when session ended
      session$onSessionEnded(function() {
        # write.csv(answers, answersFile, row.names = F)
        # write.csv(userProgress, userProgressFile, row.names = F)
      })
      
    } else {

      #Update URL
      updateQueryString(paste0("?id=", "test", "&form=", "wg", "&lang=", "pl"))
      
      #Reload session
      session$reload()

    }
    
  })#end observe
  
}#end server