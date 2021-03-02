server <- function(input, output, session) {
  
  observe({
    
    idx <<- readFromURL("id", session)
    form <<- readFromURL("form", session)
    lang <<- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(idx)){
      
      if (nchar(idx) == 21) recurrentCallSW()
      startApp(input, output, session)
      
    } else {
      
      #No URL parameters
      updateQueryString(paste0("?id=", "IlYaL6gzKieyRx92YUl1q", "&form=", "wg", "&lang=", "pl"))
      session$reload()
      
    }
    
  })
  
}