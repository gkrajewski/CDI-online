redirect <- function(parameters){
  
  tryCatch(
    
    expr = {
      
      if ("redirectionTime" %in% parameters$parameter){
        redirectionTime <- parameters[parameters$parameter=="redirectionTime", "value"]
      } else {
        redirectionTime <- 5000
      }
      
      runjs(
        paste0(
          ' setTimeout(function(){
                            window.location.replace("', parameters[parameters$parameter=="redirectionURL", "value"], '");
                          },', redirectionTime, ');
                        '
        )
      )
      
    },
    
    error = function(m){
      
      msg <- paste0("Person couldn't be redirected to the given URL.", m)
      logerror(msg)
      
    },
    
    warning = function(m){
      
      msg <- paste0("Problem with redirection to the given URL.", m)
      logwarn(msg)
      
    }
    
  )
  
}