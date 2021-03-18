server <- function(input, output, session) {
  
  availableForms <- c("wg", "ws", "iii1", "iii2", "iii3")
  availableLangs <- c("pl", "no")
  
  observe({
    
    idx <<- readFromURL("id", session)
    form <<- readFromURL("form", session)
    lang <<- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(idx)){
      
      if (is.element(form, availableForms) & is.element(lang, availableLangs)){
        startApp(input, output, session)
      } else {
        output$sidebar <- renderText({"Bad form or lang in URL"})
      }
      
    } else {
      
      # updateQueryString(paste0("?id=", "test", "&form=", "wg", "&lang=", "pl")) #IlYaL6gzKieyRx92YUl1q
      # session$reload()
      output$sidebar <- renderText({"No needed URL parameters"})
      
    }
    
  })
  
}