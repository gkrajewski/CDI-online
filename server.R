server <- function(input, output, session) {
  
  options(stringsAsFactors = FALSE)
  
  #Specify datapath and working directory path
  wd.datapath = paste0(getwd(),"/www")
  wd.init = getwd()
  
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
  
  #Load functions
  source('functions.R')
  
  #This variable can be made FALSE by lack of URL parameter or bad URL parameter
  START <<- TRUE

  #Read parameters values from URL (particular scale and version of CDI)
  observe({
    
    FORM <<- readFromURL("form", session, availableForms)
    TYPE <<- readFromURL("type", session, availableTypes)
    LANG <<- readFromURL("lang", session, availableLanguages)
    ID <<- readFromURL("id", session, availableIDs)
    
  })
  
  observe({
    
    if (START){
      
      cat("\nApplication started correctly!")
      
      #Take items and texts connected with concrete form, item type and language
      texts <- texts[texts$form == FORM & texts$item_type == TYPE & texts$language == LANG, ]
      items <- items[items$form == FORM & items$type == TYPE & items$language == LANG, ]
      
      #Specify path for answers saving
      userAnswersFile <- paste0("answers/", FORM, "/", TYPE, "/", ID, ".csv")
      
      #Render header
      output$header <- renderText({
        texts[texts$text_type == "header", "text"]
      })
      
      #Adjust rest of UI for URL parameter values
      if (FORM == "WS" & TYPE == "word"){
        multiPageUI <- TRUE
      } else {
        multiPageUI <- FALSE
      }

      if (multiPageUI){
        
        #Create list of categories
        categories <- unique(items$category)
        
        #Read answers from file if they already exists
        if (file.exists(userAnswersFile)){
          ANSWERS <<- read.csv(userAnswersFile, encoding = "UTF-8")
          print("Plik istnieje")
        } else {
          ANSWERS <<- data.frame(category = categories, items_selected = NA, comment = NA)
          print("Plik nie istnieje")
        }
        
        #Render UI
        renderMultiPageUI(texts, items, input, output, categories, userAnswersFile)
        
      } else {
        cat("\nNo UI specified yet for current combination of parameters")
      }
      
    }#end if START
    
  })#end observe

}