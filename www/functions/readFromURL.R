#Read variable from URL and check if it is valid
readFromURL <- function(parameter, session, availableValues) {
  
  value <- NULL
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query[[parameter]])){
    
    value <- query[[parameter]]
    
    if (!is.element(value, availableValues)){
      
      cat(paste0("\nERROR: Current value for parameter: ", parameter, " is: ", value, "\nAvailable values are: ", paste0(availableValues, collapse= " ")))
      START <<- FALSE
    }
    
  } else {
    
    cat(paste0("\nERROR: No value given in URL for parameter: ", parameter))
    START <<- FALSE
  }
  
  return(value)
  
}