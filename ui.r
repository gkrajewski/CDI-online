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
      
      h2(textOutput("page")), #page number for types with many pages
      div(class = "help-block", textOutput("help")), #for example some warning before saving in sidebar panel
      
      #They will be hided or shown depending on page type
      # div(id = "buttons",
          uiOutput("buttons"),
          # actionButton("backBtn", label = "Back"),
          # actionButton("nextBtn", label = "Next"),
          # actionButton("saveBtn", label = "Save")
      # )
      
    ),
    
    mainPanel(
      
      uiOutput("main"), #main content of page (input object, comment field etc.)
      
    )
    
  )
  
)