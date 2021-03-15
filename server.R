server <- function(input, output, session) {
  
  observe({
    
    idx <<- readFromURL("id", session)
    form <<- readFromURL("form", session)
    lang <<- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(idx)){
      
      startApp(input, output, session)
      
    } else {
      
      # updateQueryString(paste0("?id=", "test", "&form=", "wg", "&lang=", "pl")) #IlYaL6gzKieyRx92YUl1q
      # session$reload()
      output$sidebar <- renderText({"No needed URL parameters"})
      
    }
    
  })
  
}