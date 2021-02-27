server <- function(input, output, session) {
  
  observe({
    
    id <<- readFromURL("id", session)
    form <<- readFromURL("form", session)
    lang <<- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(id)){
      
      #if (nchar(id) == 21) recurrentCallSW()
      startApp(input, output, session)
      
      #sendmail("pkrol4478@gmail.com", "pkrol4478@gmail.com", "TYTUL MAILA", "TRESC MAILA")
      
    } else {
      
      #No URL parameters
      updateQueryString(paste0("?id=", "IlYaL6gzKieyRx92YUl1q", "&form=", "wg", "&lang=", "pl"))
      session$reload()
      
    }
    
  })
  
}