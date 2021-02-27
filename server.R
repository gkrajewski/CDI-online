server <- function(input, output, session) {
  
  observe({
    
    id <<- readFromURL("id", session)
    form <<- readFromURL("form", session)
    lang <<- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(id)){
      
      #if (nchar(idxx) == 21) recurrentCallSW()
      startApp(input, output, session)
      
      #gm_auth_configure(path = "C:/Users/pkrol/Desktop/credentials.json")
      # gm_auth_configure() 
      # gm_auth(email = TRUE, cache = ".secret")
      
    } else {
      
      #No URL parameters
      updateQueryString(paste0("?id=", "IlYaL6gzKieyRx92YUl1q", "&form=", "wg", "&lang=", "pl"))
      session$reload()
      
    }
    
  })
  
}