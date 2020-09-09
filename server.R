server <- function(input, output, session) {
  
  options(stringsAsFactors = FALSE)
  
  #Specify data paths etc.
  wd.datapath <- paste0(getwd(),"/www")
  wd.functions <- paste0(wd.datapath,"/functions")
  wd.init <- getwd()

  #Load items and texts
  setwd(wd.datapath)
  texts <- read.csv("texts.csv", encoding = "UTF-8")
  items <- read.csv("items.csv", encoding = "UTF-8")
  setwd(wd.init)
  
  #Specify available URL parameters
  availableForms <- c("WS")
  availableTypes <- c("word", "combine")
  availableLanguages <- c("Polish")
  availableIDs <- c("1234", "5678")

  #This variable can be made FALSE by lack of URL parameter or bad URL parameter
  START <<- TRUE

  #Read parameters values from URL (particular scale, version of CDI, language and user ID)
  source(paste0(wd.functions,"/readFromURL.R"))
  observe({
    
    FORM <<- readFromURL("form", session, availableForms)
    TYPE <<- readFromURL("type", session, availableTypes)
    LANG <<- readFromURL("lang", session, availableLanguages)
    ID <<- readFromURL("id", session, availableIDs)
    
  })
  
  observe({
    
    if (START){
      
      cat("\nApplication started successfully!")
      
      #Take items and texts connected with concrete CDI form, item type and language
      texts <- texts[texts$form == FORM & texts$item_type == TYPE & texts$language == LANG, ]
      items <- items[items$form == FORM & items$type == TYPE & items$language == LANG, ]
      
      #Specify path for answers saving
      userAnswersFile <- paste0("answers/", FORM, "/", TYPE, "/", ID, ".csv")
      
      #Render header
      output$header <- renderText({
        texts[texts$text_type == "header", "text"]
      })
      
      #Adjust rest of UI for URL parameters values
      if (FORM == "WS" & TYPE == "word"){

        # #Create list of categories
        # categories <- unique(items$category)
        # 
        # #Read answers
        # ANSWERS <<- readAnswers(userAnswersFile, "categories")
        # 
        # #Render UI
        # renderMultiPageUI(texts, items, input, output, categories, userAnswersFile)
        
        #Render UI for type 'word'
        source(paste0(wd.functions,"/renderMultiPageUI.R"))
        renderMultiPageUI(input, userAnswersFile, items, texts, output)
        
        
      } else {
  
        #Render UI for type 'combine'
        source(paste0(wd.functions,"/renderOnePageUI.R"))
        renderOnePageUI(input, userAnswersFile, items, texts, output)        
        
      }
      
    }#end if START
    
  })#end observe

}