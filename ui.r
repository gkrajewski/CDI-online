ui <- fluidPage(

  #Needed to be able to use shinyjs library
  useShinyjs(), 
  tags$script(src = "disabler_1_10_01.js"),
  
  #Set main UI theme
  theme = shinytheme("flatly"),
  
  #Load style and fonts
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&display=swap", rel="stylesheet")
  ),
  
  div(id="version", "v1.10.01"),
  h1(textOutput("cdiNamePrefix")), 
  h2(withSpinner(textOutput("cdiNameSufix"), type=2, color.background = '#FFF', proxy.height = "50px")),
  uiOutput("menu"),
  h3(textOutput("header")), #name of current type
  
  #Main layout
  sidebarLayout(
    
    sidebarPanel(
      
      uiOutput("sidebar")

    ),
    
    mainPanel(
      
      uiOutput("main")
      
    ),
    
    position = c("right")
    
  ),
  
  #For showing nice disconnect message
  uiOutput("dcMessage")
  
)
