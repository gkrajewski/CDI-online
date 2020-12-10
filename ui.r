ui <- fluidPage(
  
  #It makes shinyjs library work
  useShinyjs(), 
  
  #Loads ready UI theme
  theme = shinytheme("flatly"),
  
  #Loads css file with custom apperance settings
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css")
  ),
  
  h1(textOutput("cdiNamePrefix")),
  h2(textOutput("cdiNameSufix")),
  uiOutput("menu"),
  h3(textOutput("header")), #Current type name
  
  sidebarLayout(
    
    sidebarPanel(
      
      uiOutput("sidebar")

    ),
    
    mainPanel(
      
      uiOutput("main"), #main content of page (input object, comment field etc.)
      
    )
    
  )
  
)