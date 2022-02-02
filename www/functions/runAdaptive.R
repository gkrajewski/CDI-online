runAdaptive <- function(input, output, session, lang, form, idx, run, urlString, fromSW){
  
  #Read translations & settings
  typePath <- paste0(LANGUAGES_PATH, "/", lang, "/forms/adaptive")
  formPath <- paste0(typePath, "/", form)
  txt <- readInputFile(output = output, path = formPath, fileName = "settings&translations.csv")
  
  #Load type-universal translations & settings (if they exists)
  if (file.exists(paste0(typePath, "/uniSettings&Translations.csv"))){
    
    #Merge form-specific translations with type-universal translations
    uniTxt <- readInputFile(output = output, path = typePath, fileName = "uniSettings&Translations.csv")
    txt <- mergeTranslations(txt, uniTxt, output, "adaptive")
    
  }
  
  if (!is.null(txt)){
    
    #Add rendering nice message when error
    output$dcMessage <- renderUI({disconnectMessage(text = paste0(txt[txt$text_type == "error", "text"], " [", urlString, "]"), refresh = txt[txt$text_type == "refresh", "text"])})
    
    #Render CDI name
    output$cdiNamePrefix <- renderText({txt[txt$text_type == "cdiNamePrefix", "text"]})
    output$cdiNameSufix <- renderText({txt[txt$text_type == "cdiNameSufix", "text"]})
    
    #Prepare subject
    subjectFile <- paste0("CATsubjects/", urlString, ".rds")
    
    if (file.exists(subjectFile)){
      subject <- readRDS(subjectFile)
      
    } else {
      
      #First contact with the form
      if (fromSW) recurrentCallSW(idx, form, lang)
      
      subject <- list(id = idx, gender = NA, birth = NA, filler = NA, form = form, formEnded = FALSE)
      
      if ("groups" %in% txt$text_type) {
        groups<-strsplit(txt[txt$text_type == "groups", "text"], ",")[[1]]
      } else {
        groups <- c("group1")
      }
      
      for (subgroup in groups) {
        subject[[paste0(subgroup, "Test")]] = NA
        subject[[paste0(subgroup, "Theta")]] = NA
        subject[[paste0(subgroup, "Start")]] = NA
        subject[[paste0(subgroup, "Comment")]] = NA
        subject[[paste0(subgroup, "CommentEnd")]] = NA
      }
  
    }
    
    #Render rest of UI
    if (is.na(subject$birth) | is.na(subject$gender) | is.na(subject$filler)){
      
      ### RENDER DEMOGRAPHIC PAGE ###
      
      #Main panel
      output$main <- renderUI({
        list(
          dateInput(
            "birth",
            txt[txt$text_type == "birthQuestion", "text"]
          ),
          radioButtons(
            "gender",
            label = txt[txt$text_type == "genderQuestion", "text"],
            selected = character(0),
            choices = strsplit(txt[txt$text_type == "genders", "text"], ",")[[1]]
          ),
          radioButtons(
            "filler",
            label = txt[txt$text_type == "fillerQuestion", "text"],
            selected = character(0),
            choices = strsplit(txt[txt$text_type == "fillers", "text"], ",")[[1]]
          ),
          textInput("fillerTxt", label = NULL, value = character(0))
        )
      })
      
      #Sidebar
      output$sidebar <- renderUI({
        actionButton("btn", label = txt[txt$text_type == "btn", "text"], class = "btn-primary")
      })
      
      #Awaiting confirmation
      observeEvent(input$btn, {
        
        if (is.null(input$gender)){
          
          showModal(modalDialog(
            title = txt[txt$text_type == "modalTitle", "text"],
            txt[txt$text_type == "noGender", "text"],
            easyClose = TRUE,
            footer = NULL
          ))
          
        } else if (is.null(input$filler)) {
          
          showModal(modalDialog(
            title = txt[txt$text_type == "modalTitle", "text"],
            txt[txt$text_type == "noFiller", "text"],
            easyClose = TRUE,
            footer = NULL
          ))
          
        } else {
          
          subject$birth <- paste(input$birth)
          subject$gender <- input$gender
          subject$filler <- input$filler
          subject$fillerTxt <- input$fillerTxt
          loginfo(paste0(urlString, " starting test from the beginning."))
          startTest(input, output, session, subject, formPath, subjectFile, lang, idx, form, txt, urlString, fromSW)
          
        }
        
      })
      
    } else {
      
      ### THERE WAS CONTACT WITH THE TEST ALREADY ###
      
      if (subject[["formEnded"]]){

        #Test already filled
        showModal(modalDialog(
          txt[txt$text_type == "alreadyFilled", "text"],
          easyClose = FALSE,
          footer = NULL
        ))
        
      } else {
        
        #Test started but not filled
        loginfo(paste0(urlString, " continuing with already started test."))
        startTest(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, urlString, fromSW)
        
      }
      
    }    
    
  }
  
}
