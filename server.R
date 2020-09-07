server <- function(input, output, session) {
  
  options(stringsAsFactors = FALSE)
  
  #Specify datapath and working directory path
  wd.datapath = paste0(getwd(),"/www")
  wd.init = getwd()
  
  #Specify available URL parameters
  availableForms <- c("WS")
  availableTypes <- c("word")
  availableLanguages <- c("Polish")
  availableIDs <- c("1234", "5678")
  
  #Load functions
  source('functions.R')
  
  #Variable to use for layouts with several pages
  page <- reactiveVal(0)
  
  observe({
    
    #This variable will be FALSE when there are some errors connected with URL parameters
    CAN_START <<- TRUE
    
    #Read variables from URL
    form <- readFromURL("form", session, availableForms)
    type <- readFromURL("type", session, availableTypes)
    lang <- readFromURL("lang", session, availableLanguages)
    userID <- readFromURL("id", session, availableIDs)
    
    if (CAN_START){

      setwd(wd.datapath)

      #Read texts
      texts <- read.csv("texts.csv", encoding = "UTF-8")
      texts <- texts[texts$form == form & texts$item_type == type & texts$language == lang, ]
      
      #Read items
      items <- read.csv("items.csv", encoding = "UTF-8")
      items <- items[items$form == form & items$type == type & items$language == lang, ]
      
      setwd(wd.init)

      #Render header
      output$header <- renderText({
        texts[texts$text_type == "header", "text"]
      })
      
      if (form == "WS" & type == "word") {
        
        output$sidebar <- renderTwoButtonsSidebar(texts)
        
        categories <- unique(items$category)
        MAX_PAGE <<- length(categories) + 1
        
        #Load current input if it exists
        USER_ANSWERS_FILE <<- paste0("answers/", form, "/", type, "/", userID, ".csv")
          
        #Read answers from file if it exists
        if (file.exists(USER_ANSWERS_FILE)){
          ANSWERS <<- read.csv(USER_ANSWERS_FILE, encoding = "UTF-8")
          print("Plik istnieje")
        } else {
          ANSWERS <<- data.frame(category = categories, items_selected = NA, comment = NA)
          print("Plik nie istnieje")
        }
        
        #Create UI depending on page
        if (page() == 0){
          
          #Welcome page
          INPUT_PAGE <<- FALSE
          
          output$main <- renderUI({
            
            list(
              
              createProgressBar(page()),
              texts[texts$text_type == "instr", "text"]
              
            )
            
          })
          
        } else if (page() == MAX_PAGE) {
          
          #Thanks page
          INPUT_PAGE <<- FALSE
          
          output$main <- renderUI({
            
            list(
              
              createProgressBar(page()),
              texts[texts$text_type == "thanks", "text"]
              
            )
            
          })
          
        } else {
          
          #Input page
          INPUT_PAGE <<- TRUE
          CURR_CAT <<- categories[page()]
          
        }
        
      }
      
    }#end big if

  })#end observe

  #Add buttons service
  observeEvent(input$nextB, {

    if (page() < MAX_PAGE){

      newPage <- page() + 1
      page(newPage)

    }

    if (INPUT_PAGE){
      saveData()
    }

  })

  observeEvent(input$prevB, {

    if (page() > 0){
      
      newPage <- page() - 1
      page(newPage)
      
    }

    if(INPUT_PAGE){
      saveData()
    }

  })

  # output$main <- renderUI({
  #   
  #   #Welcome page
  #   if (page() == 0){
  #     
  #     inputPage <<- FALSE
  #     
  #     list(
  #       
  #       createProgressBar(),
  #       
  #       p (
  #         
  #         class = "instr",
  #         texts$instr
  #         
  #       )
  #       
  #     )
  #     
  #   }
  #   
  #   #Thanks page
  #   else if (page() == maxPage) {
  #     
  #     inputPage <<- FALSE
  #     
  #   }
  #   
  #   #Input page
  #   else {
  #     
  #     inputPage <<- TRUE
  #     currCat <<- categories[page()]
  #     selectedStr <- as.character(answers[answers$category == currCat, "items_selected"])
  #     comment <- answers[answers$category == currCat, "comment"]
  #     
  #     if (!is.na(selectedStr)){
  #       selected <- strsplit(selectedStr, " ")[[1]]
  #     } else {
  #       selected <- NULL
  #     }
  #     
  #     if (is.na(comment)){
  #       comment = ""
  #     }
  #     
  #     list(
  #       
  #       createProgressBar(),
  #       
  #       h4(texts[[currCat]]),
  #       
  #       br(),
  #       
  #       div(
  #         
  #         class = "itemsContainer",
  #         
  #         checkboxGroupInput(
  #           "items",
  #           selected = selected,
  #           label = NULL,
  #           choiceNames = as.character(items[items$category == currCat, ]$definition),
  #           choiceValues = as.character(items[items$category == currCat, ]$item_id)
  #         )
  #         
  #       ),
  #       
  #       div (
  #         
  #         class = "commentContainer",
  #         
  #         textAreaInput("comment", label = texts$comments, value = comment)
  #         
  #       )
  #       
  #     )#end list
  #     
  #   }
  #   
  # })#end renderUI
  
}