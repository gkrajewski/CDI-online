options(stringsAsFactors = FALSE)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#install.packages("shinythemes")
#install.packages("shiny")
library(shinythemes)
library(shiny)

#Load Polish texts
lang <- "Polish"
wd.datapath = paste0(getwd(),"/www")
wd.init = getwd()
setwd(wd.datapath)
texts = read.csv("texts.csv", encoding = "UTF-8")
texts <- texts[texts$language == lang,]
setwd(wd.init)

ui <- fluidPage(
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css")
  ),
  
  theme = shinytheme("flatly"),
  h1(texts$header),
  
  sidebarLayout(
    
    sidebarPanel(
      
      p(class = "help-block", texts$help_block),
      
      div(
        
        id = "buttons",
        
        actionButton(
          inputId = "prevB", 
          label = texts$prev_button
        ),
        
        actionButton(
          inputId = "nextB", 
          label = texts$next_button
        )
      )
      
    ),
    
    mainPanel(
      
      uiOutput("main")
      
    )
    
  )
)

server <- function(input, output, session) {
  
  #Create reactive variable 'page'
  page <- reactiveVal(0)
  
  #Load items from csv file
  setwd(wd.datapath)
  items = read.csv("items.csv", encoding = "UTF-8")
  setwd(wd.init)
  
  #Create list of categories
  categories <- unique(items$category)

  #Load current input if it exists
  if (file.exists("answers.csv")){
    answers = read.csv("answers.csv", encoding = "UTF-8")
  } else {
    answers <- data.frame(category = categories, items_selected = NA, comment = NA)
  }
  
  #Save data
  saveData <- function(){
    
    answers[answers$category == currCat,"items_selected"] <<- paste(input$items, collapse =  " ")
    answers[answers$category == currCat,"comment"] <<- input$comment
    write.csv(answers, file = "answers.csv", row.names = F)
    
  }
  
  #Create progress bar
  createProgressBar <- function(){
    
    percent <- paste0(as.character(round(page()/maxPage*100)),"%")
    return(div(class="progress", div(class="progress-bar", style=paste0("width: ",percent), percent)))
    
  }
  
  #Add buttons service
  observeEvent(input$nextB, {
    
    if (page() < maxPage){
      
      newPage <- page() + 1
      page(newPage)
      
    }
    
    if (inputPage){
      saveData()
    }
    
  })
  
  observeEvent(input$prevB, {
    
    if (page() > 0){
      newPage <- page() - 1
      page(newPage)
    }
    
    if(inputPage){
      saveData()
    }
    
  })
  
  #Create UI depends on page
  inputPage <- FALSE
  currCat <- NULL
  maxPage <- length(categories) + 1
  
  output$main <- renderUI({
    
    #Welcome page
    if (page() == 0){
      
      inputPage <<- FALSE
      
      list(
        
        createProgressBar(),
        
        p (
          
          class = "instr",
          texts$instr
          
        )
        
      )
      
    }
    
    #Thanks page
    else if (page() == maxPage) {
      
      inputPage <<- FALSE
      
      list (
        
        createProgressBar(),
        p(texts$thanks)
        
      )

    }
    
    #Input page
    else {
      
      inputPage <<- TRUE
      currCat <<- categories[page()]
      selectedStr <- answers[answers$category == currCat, "items_selected"]
      comment <- answers[answers$category == currCat, "comment"]
      
      if (!is.na(selectedStr)){
        selected <- strsplit(selectedStr, " ")[[1]]
      } else {
        selected <- NULL
      }
      
      if (is.na(comment)){
        comment = ""
      }

      list(
        
        createProgressBar(),
        
        h4(texts[[currCat]]),
        
        br(),
        
        div(
          
          class = "itemsContainer",
          
          checkboxGroupInput(
            "items",
            selected = selected,
            label = NULL,
            choiceNames = as.character(items[items$category == currCat, ]$definition),
            choiceValues = as.character(items[items$category == currCat, ]$item_id)
          )
          
        ),
        
        div (
          
          class = "commentContainer",
          
          textAreaInput("comment", label = texts$comments, value = comment)
          
        )
        
      )#end list
      
    }
    
  })#end renderUI

  
}#end server

shinyApp(ui = ui, server = server)