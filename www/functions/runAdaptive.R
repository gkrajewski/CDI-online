runAdaptive <- function(input, output, session, lang, form, idx, run){

  urlString <- paste(lang, form, idx, run, sep = "-")
  #Load translations
  testPath <- paste0(LANGUAGES_PATH, "/", lang, "/forms/", form)
  print(testPath)
  setwd(testPath)
  txt <- read.csv(paste0("translations.csv"), encoding = "UTF-8", sep = ";", strip.white = T)
  print("step2")
  setwd(INIT_PATH)
  print("step3")
  
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
        output$warning <- renderText({txt[txt$text_type == "noGender", "text"]})
      } else if (is.null(input$filler)) {
        output$warning <- renderText({txt[txt$text_type == "noFiller", "text"]})
      } else {
        output$warning <- renderText({})
        subject$birth <- paste(input$birth)
        subject$gender <- input$gender
        subject$filler <- input$filler
        subject$fillerTxt <- input$fillerTxt
        startTest(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt)
      }
    })
    
  } else {
    
    if (!is.na(subject[["testEnd"]])){
      output$sidebar <- renderUI({div(class = "help-block", txt[txt$text_type == "thanks", "text"])}) #End of all testing
    } else {
      startTest(input, output, session, subject, testPath, subjectFile, lang, idx, form, txt) #Start test
    }
    
  }
    
}
