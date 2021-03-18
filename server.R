server <- function(input, output, session) {
  
  availableLangs <- list.files(path = languagesPath, recursive = FALSE)
  
  observe({
    
    #Read parameters values from URL
    idx <<- readFromURL("id", session)
    form <<- readFromURL("form", session)
    lang <<- readFromURL("lang", session)
    
    if (!is.null(form) & !is.null(lang) & !is.null(idx)){
      
      if (is.element(lang, availableLangs)){
        
        langPath <<- paste0(languagesPath, "/", lang)
        availableForms <- list.files(path = paste0(langPath, "/forms"), recursive = FALSE)
        
        if (is.element(form, availableForms)){
          
          formPath <<- paste0(languagesPath, "/", lang, "/forms/", form)
          startInventory(input, output, session)
          
        } else {
          
          output$sidebar <- renderText({paste0(c("Bad value of form parameter in URL. Accesible values are: ", availableForms), collapse = " ")})
          
        }
        
        
      } else {
        
        output$sidebar <- renderText({paste0(c("Bad value of lang parameter in URL. Accesible values are: ", availableLangs), collapse = " ")})
        
      }
      
    } else {
      
      # updateQueryString(paste0("?id=", "test", "&form=", "wz", "&lang=", "pl")) #IlYaL6gzKieyRx92YUl1q #id=test&form=wg&lang=pl
      # session$reload()
      output$sidebar <- renderText({"No needed params in URL (lang, form and id)"})
      
    }
    
  })
  
}