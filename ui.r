ui <- fluidPage(

  useShinyjs(), 
  
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Lato:wght@300;700&display=swap", rel="stylesheet")
  ),
  
  h1(textOutput("cdiNamePrefix")),
  h2(textOutput("cdiNameSufix")),
  uiOutput("menu"),
  h3(textOutput("header")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      uiOutput("sidebar")

    ),
    
    mainPanel(
      
      uiOutput("main"),
      textOutput("warning")
      
    ),
    position = c("right")
    
  )
  
)