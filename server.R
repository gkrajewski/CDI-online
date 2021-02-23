server <- function(input, output, session) {
  
  observe({
    
    id <- readFromURL("id", session)
    form <- readFromURL("form", session)
    lang <- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(id)){
      
      if (nchar(id) == 21){
        fromSW <- TRUE
      } else {
        fromSW <- FALSE
      }
      
      if (!fromSW){
        
        startApp(input, output, session, id, form, lang)
        
      } else {
        
        if (callSW(form = form, id = id, return = FALSE)){
          
          startApp(input, output, session, id, form, lang)
          
        } else {
          
          output$sidebar <- renderUI({"Trying to connect with StarWords app"})
          
          delay(
            
            3000,
            if (callSW(form = form, id = id, return = TRUE)){
              startApp(input, output, session, id, form, lang)
            } else {
              output$sidebar <- renderUI({"Cannot connect with StarWords app"})
            }
            
          )
          
        }
        
      }
      
    } else {
      
      #No URL parameters
      updateQueryString(paste0("?id=", "V1StGXR8_Z5jdHi6B-myT", "&form=", "wg", "&lang=", "pl"))
      session$reload()
      
    }
    
  })
  
}