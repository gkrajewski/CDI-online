library(shinythemes)

ui <- fluidPage(
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css")
  ),
  
  theme = shinytheme("flatly"),
  h1(textOutput("header")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      uiOutput("sidebar")
      
    ),
    
    mainPanel(
      
      uiOutput("main"),
      textOutput("info")
      
    )
    
  )
)