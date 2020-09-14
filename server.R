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
  
  #Specify available URL values for parameters 'id' and 'form'
  availableIDs <- c("1234", "5678")
  availableForms <- unique(texts$form)

  #Read function
  source(paste0(wd.functions,"/readFromURL.R"))

  observe({
    
    #Read parameters values from URL
    id <- readFromURL("id", session, availableIDs)
    form <- readFromURL("form", session, availableForms)
    
    availableTypes <- unique(texts[texts$form == form, 'item_type'])
    type <- readFromURL("type", session, availableTypes)
    
    availableLanguages <- unique(texts[texts$item_type == type & texts$form == form, 'language'])
    lang <- readFromURL("lang", session, availableLanguages)
    
    #Render UI if all parameters values are correct
    if (!is.null(form) & !is.null(type) & !is.null(lang) & !is.null(id)){
      
      #Take items and texts connected with concrete CDI form, item type and language
      texts <- texts[texts$form == form & texts$item_type == type & texts$language == lang, ]
      items <- items[items$form == form & items$type == type & items$language == lang, ]
      
      #Specify path for file with answers
      userAnswersFile <- paste0("answers/", form, "/", type, "/", id, ".csv")

      #Render header
      output$header <- renderText({
        texts[texts$text_type == "header", "text"]
      })
      
      #Read functions
      source(paste0(wd.functions,"/renderSidebar.R"))
      source(paste0(wd.functions,"/renderMain.R"))

      #Render sidebar
      output$sidebar <- renderSidebar(type, texts)
      
      #Render main panel
      output$main <- renderMain(wd.functions, type, input, output, items, texts, userAnswersFile)

    } else {
      
      output$sidebar <- renderText({"Error: No value of necessary URL parameter or bad value."})
      
    }

  })#end observe

}