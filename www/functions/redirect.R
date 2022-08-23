redirect <- function(parameters, idx, lang, form, type, run){
  
  tryCatch(
    
    expr = {
      
      if ("redirectionTime" %in% parameters$parameter){
        redirectionTime <- parameters[parameters$parameter=="redirectionTime", "value"]
      } else {
        redirectionTime <- 0
      }
      
      redirectionURL <- parameters[parameters$parameter=="redirectionURL", "value"]
      
      if ("redirectionParams" %in% parameters$parameter){
        
        redirectionURL <- paste0(redirectionURL, "/?")
        paramsList <- as.list(strsplit(parameters[parameters$parameter=="redirectionParams", "value"], ",")[[1]]) #Split into list based on comma
        paramsList <- gsub(" ", "", paramsList) #Remove whitespaces
        firstParam <- TRUE
        
        for (param in paramsList){
          if (!firstParam) redirectionURL <- paste0(redirectionURL, "&")
          if (param == "id") redirectionURL <- paste0(redirectionURL, "id=", idx)
          if (param == "form") redirectionURL <- paste0(redirectionURL, "form=", form)
          if (param == "lang") redirectionURL <- paste0(redirectionURL, "lang=", lang)
          if (param == "type") redirectionURL <- paste0(redirectionURL, "type=", type)
          if (param == "run") redirectionURL <- paste0(redirectionURL, "run=", run)
          firstParam <- FALSE
        }
        
      } 
      
      runjs(
        paste0(
          ' setTimeout(function(){
                            window.location.replace("', redirectionURL, '");
                          },', redirectionTime, ');
          '
        )
      )
      
    },
    
    error = function(m){
      
      msg <- paste0("Problem with redirection to the given URL.", m)
      logerror(msg)
      
    },
    
    warning = function(m){
      
      msg <- paste0("Problem with redirection to the given URL.", m)
      logwarn(msg)
      
    }
    
  )
  
}