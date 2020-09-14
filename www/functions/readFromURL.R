#Read parameter value from URL and check if it is available value
readFromURL <- function(parameter, session, availableValues) {
  
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query[[parameter]])){
    
    value <- query[[parameter]]
    if (is.element(value, availableValues)) return(value)
    
  } 
  
  return(NULL)
  
}