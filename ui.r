library(shinythemes)

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