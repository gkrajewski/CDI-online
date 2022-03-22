redirect <- function(parameters, idx){
  
  tryCatch(
    
    expr = {
      
      if ("redirectionTime" %in% parameters$parameter){
        redirectionTime <- parameters[parameters$parameter=="redirectionTime", "value"]
      } else {
        redirectionTime <- 5000
      }
      
      redirectionURL <- parameters[parameters$parameter=="redirectionURL", "value"]
      
      if ("redirectionId" %in% parameters$parameter){
        
        if (parameters[parameters$parameter=="redirectionId", "value"] == "yes") {
          redirectionURL <- paste0(redirectionURL, "/?id=", idx)
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
      
      msg <- paste0("Person couldn't be redirected to the given URL.", m)
      logerror(msg)
      
    },
    
    warning = function(m){
      
      msg <- paste0("Problem with redirection to the given URL.", m)
      logwarn(msg)
      
    }
    
  )
  
}