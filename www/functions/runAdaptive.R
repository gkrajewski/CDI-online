runAdaptive <- function(input, output, session, lang, form, idx, run){

  urlString <- paste(lang, form, idx, run, sep = "-")

  #Load settings
  testPath <- paste0(LANGUAGES_PATH, "/", lang, "/forms/adaptive/", form)
  print(testPath)
  setwd(testPath)
  uniTransl <- read.csv(paste0("../uniCATsettings.csv"), encoding = "UTF-8", sep = ";", strip.white = T)
  transl <- read.csv(paste0("settings.csv"), encoding = "UTF-8", sep = ";", strip.white = T)
  
  translID <- paste(transl$text_type, transl$text)
  uniTranslID <- paste(uniTransl$text_type, uniTransl$text)
  uniTransl <- subset(uniTransl, !(uniTranslID %in% translID)) #Get things from uniTransl (uniCATsettings) that are not in translations
  txt <- rbind(uniTransl, transl)
  
  setwd(INIT_PATH)
  
  #Render CDI name
  output$cdiNamePrefix <- renderText({txt[txt$text_type == "cdiNamePrefix", "text"]})
  output$cdiNameSufix <- renderText({txt[txt$text_type == "cdiNameSufix", "text"]})
  
  #Prepare subject variable
  subjectFile <- paste0("subjects/", urlString, ".rds")
  if (file.exists(subjectFile)){
    subject <- readRDS(subjectFile)
  } else {
    subject <- list(id = idx, gender = NA, birth = NA, test = NA, testEnd = NA, filler=NA)
    
    if ("groups" %in% txt$text_type) {
      groups<-strsplit(txt[txt$text_type == "groups", "text"], ",")[[1]]
    } else {
      groups <- c("group1")
    }
    
    for (subgroup in groups) {
      subject[[paste0(subgroup, "Test")]] = NA
      subject[[paste0(subgroup, "Theta")]] = NA
      subject[[paste0(subgroup, "Start")]] = NA
    }
    
    recurrentCallSW(idx, form, lang)
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
    
    #Awaiting answer
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
        output$warning <- renderText({})
        subject$birth <- paste(input$birth)
        subject$gender <- input$gender
        subject$filler <- input$filler
        subject$fillerTxt <- input$fillerTxt
        loginfo(paste0(urlString, " starting test from the beginning."))
        startTest(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, urlString)
      }
    })
    
  } else {
    
    if (!is.na(subject[["testEnd"]])){
      
      #Test already filled
      showModal(modalDialog(
        txt[txt$text_type == "alreadyFilled", "text"],
        easyClose = FALSE,
        footer = NULL
      ))
      
    } else {
      loginfo(paste0(urlString, " continuing with the started test."))
      startTest(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt, urlString) #Start test
    }
    
  }
    
}
