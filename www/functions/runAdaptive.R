runAdaptive <- function(input, output, session, lang, form, idx, run, urlString, fromSW){
  
  #Read translations & settings
  inputFilesRead <- tryCatch(
    
    expr = {
      
      #Load settings and translations
      testPath <- paste0(LANGUAGES_PATH, "/", lang, "/forms/adaptive/", form)
      setwd(testPath)
      uniTransl <- read.csv(paste0("../uniTranslations.csv"), encoding = "UTF-8", sep = ";", strip.white = T)
      transl <- read.csv(paste0("translations.csv"), encoding = "UTF-8", sep = ";", strip.white = T)
      parameters <- read.csv(paste0("parameters.csv"), encoding = "UTF-8", sep = ";", strip.white = T)
      
      #Prepare settings and translations
      translID <- paste(transl$text_type, transl$text)
      uniTranslID <- paste(uniTransl$text_type, uniTransl$text)
      uniTransl <- subset(uniTransl, !(uniTranslID %in% translID)) #Get things from uniTransl (uniSettings&translations) that are not in translations
      txt <- rbind(uniTransl, transl)
      
      setwd(INIT_PATH)
      
      loginfo("Translation input files were read in")
      
      TRUE
      
    },
    
    error = function(m){
      
      msg <- paste0("There is problem with input file <br><br>", m)
      logerror(msg)
      output$sidebar <- renderText({msg})
      return(FALSE)
      
    },
    
    warning = function(m){
      
      msg <- paste0("There is problem with input file <br><br>", m)
      logwarn(msg)
      output$sidebar <- renderText({msg})
      return(FALSE)
      
    }
    
  )

  if (inputFilesRead){
    
    #Add rendering nice message when error
    output$dcMessage <- renderUI({disconnectMessage(text = paste0(txt[txt$text_type == "error", "text"], " [", urlString, "]"), refresh = txt[txt$text_type == "refresh", "text"])})
    
    #Render CDI name
    output$cdiNamePrefix <- renderText({txt[txt$text_type == "cdiNamePrefix", "text"]})
    output$cdiNameSufix <- renderText({txt[txt$text_type == "cdiNameSufix", "text"]})
    
    #Prepare subject
    subjectFile <- paste0("CATsubjects/", urlString, ".rds")
    
    if (file.exists(subjectFile)){
      subject <- readRDS(subjectFile)
      loginfo("Subject file was read in")
      
    } else {
      
      #First contact with the form
      if (fromSW) recurrentCallSW(idx, form, lang)
      
      subject <- list(id = idx, gender = NA, birth = NA, filler = NA, form = form, formEnded = FALSE)
      
      if ("groups" %in% parameters$parameter) {
        groups<-strsplit(parameters[parameters$parameter == "groups", "value"], ",")[[1]]
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
      
      loginfo("Subject file was created")
  
    }
    
    #Render rest of UI
    if (is.na(subject$birth) | is.na(subject$gender) | is.na(subject$filler)){
      
      ### RENDER DEMOGRAPHIC PAGE ###
      
      #Main panel
      output$main <- renderUI({
        list(
          if (is.element("instr", txt$text_type)) h5(txt[txt$text_type == "instr", "text"]),
          if (is.element("longText", txt$text_type)) HTML(txt[txt$text_type == "longText", "text"]),
          if (is.element("warning", txt$text_type)) p(class = "warning", strong(txt[txt$text_type == "warning", "text"])),
          dateInput(
            "birth",
            txt[txt$text_type == "dateLabel", "text"],
            language = lang
          ),
          radioButtons(
            "gender",
            label = txt[txt$text_type == "genderLabel", "text"],
            selected = character(0),
            choices = strsplit(txt[txt$text_type == "genders", "text"], ",")[[1]]
          ),
          radioButtons(
            "filler",
            label = txt[txt$text_type == "fillerLabel", "text"],
            selected = character(0),
            choices = strsplit(txt[txt$text_type == "fillers", "text"], ",")[[1]]
          ),
          textInput("fillerTxt", label = NULL, value = character(0))
        )
      })
      
      #Sidebar
      output$sidebar <- renderUI({
        list(
          if (is.element("firstPageSidebarInstr", txt$text_type)) div(class="help-block", txt[txt$text_type == "firstPageSidebarInstr", "text"]),
          actionButton("startBtn", label = txt[txt$text_type == "startBtn", "text"], class = "btn-primary")
        )
      })
      
      #Awaiting confirmation
      observeEvent(input$startBtn, {
        
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
          startTest(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, parameters, urlString, fromSW, run)
        }
        
      }, once = TRUE)
      
    } else {
      
      ### THERE WAS CONTACT WITH THE TEST ALREADY ###
      
      if (subject[["formEnded"]]){
        
        endMsgtxt <- txt[txt$text_type == "alreadyFilled", "text"]
        additionalMessage <- parameters[parameters$parameter=="additionalEndMessageFromDatabase", "value"]
        
        if (length(additionalMessage)>0 && additionalMessage=="yes") {
          additionalMessageTxt <- getAdditionalEndMessage(urlString, "database", parameters, txt)
          endMsgtxt <- paste(endMsgtxt, "<br><br>", additionalMessageTxt)
        }
        
        showModal(modalDialog(
          title = txt[txt$text_type == "thanksMsgTitle", "text"],
          HTML(endMsgtxt),
          easyClose = FALSE,
          footer = NULL
        ))
        
      } else {
        
        #Test started but not filled
        loginfo(paste0(urlString, " continuing with already started test."))
        startTest(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, parameters, urlString, fromSW)
        
      }
      
    }    
    
  }
  
}
