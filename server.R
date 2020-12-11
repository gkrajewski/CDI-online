server <- function(input, output, session) {
  
  observe({
    
    #Read parameters values from URL
    id <- readFromURL("id", session, availableIds)
    form <- readFromURL("form", session, availableForms)
    lang <- readFromURL("lang", session, availableLanguages)
    
    #Render UI if all URL parameters are correct
    if (!is.null(form) & !is.null(lang) & !is.null(id)){
      
      #Get translations, items and settings connected with given language and form
      txt <<- translations[translations$language == lang & (translations$form == form | translations$form == ""),]
      items <<- allItems[allItems$language == lang & allItems$form == form,] 
      settings <<- allSettings[allSettings$language == lang & allSettings$form == form,]
      enableSettings <<- allEnableSettings[allEnableSettings$language == lang & allEnableSettings$form == form,]
    
      #Render CDI name
      output$cdiNamePrefix <- renderText({txt[txt$text_type == "cdiNamePrefix", "text"]})
      output$cdiNameSufix <- renderText({txt[txt$text_type == "cdiNameSufix", "text"]})
      
      #Get types from enableSettings file
      types <<- enableSettings$type
      
      #Bind some of types and create artificial categories as specified in enableSettings file
      for (type in types){
        
        if (enableSettings[enableSettings$type == type, "binded_types"] != "none"){
          
          if (enableSettings[enableSettings$type == type, "binded_types"] == "startWith"){
            
            items[substr(allItems$type, 1, nchar(type)) == type, "category"] <<- allItems[substr(allItems$type, 1, nchar(type)) == type, "type"]
            items[substr(allItems$type, 1, nchar(type)) == type, "type"] <<- type
            
          } else {
            
            bindedTypes <- strsplit(enableSettings[enableSettings$type == type, "binded_types"], ",")[[1]]
            
            for (bindedType in bindedTypes){
              
              items[substr(allItems$type, 1, nchar(bindedType)) == bindedType, "category"] <<- allItems[substr(allItems$type, 1, nchar(bindedType)) == bindedType, "type"]
              items[substr(allItems$type, 1, nchar(bindedType)) == bindedType, "type"] <<- type
              
            }
            
          }
          
        }
        
      }
      
      #Get number of types
      typesNr <- length(types)
      
      #Prepare progress file
      progressFile <- paste0("progress/", lang, "-", form, "-", id, ".csv")
      
      if (file.exists(progressFile)){
        
        progress <<- read.csv(progressFile, encoding = "UTF-8")
        
      } else {

        firstCat <- unique(items[items$type == types[1], "category"])[1] #get first category of first type
        if (is.na(firstCat)) firstCat <- "none"
        
        progress <<- data.frame(
          type = types,
          done = FALSE,
          disabled = enableSettings$initially_disabled,
          current = c(TRUE, rep(FALSE, typesNr - 1)), #make 1st type as current
          category = c(firstCat, rep("none", typesNr - 1)) 
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
      
      #Fill in list of type buttons divs
      lapply(1:typesNr, function(i) {

        type <- types[i]
        
        #Prepare css class for div with button
        class <- "menuButtonContainer"
        if (progress[progress$type == type, "done"]) class <- paste(class, "menuButtonContainerDone")
        if (progress[progress$type == type, "current"]) class <- paste(class, "menuButtonContainerActive")
        
        #Prepare button div
        buttonDiv <- div(id = paste0(type, "container"), class = class, actionButton(type, label = txt[txt$text_type == paste0(type,"Btn"), "text"], class = "btn-primary"))
        if (progress[progress$type == type, "disabled"]) buttonDiv <- disabled(buttonDiv)
        
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
      type <- types[match(TRUE, progress$current)] #get current type according to progress df
      renderType(input, output, type)
      
      #Add observers (sidebar buttons and input objects)
      addObservers(input, output)
      
      #Save answers and progress to csv file when session ended
      session$onSessionEnded(function() {
        # write.csv(answers, answersFile, row.names = F)
        # write.csv(progress, progressFile, row.names = F)
      })
      
    } else {

      #Update URL
      updateQueryString(paste0("?id=", "test", "&form=", "WG", "&lang=", "Polish"))
      
      #Reload session
      session$reload()

    }
    
  })#end observe
  
}#end server