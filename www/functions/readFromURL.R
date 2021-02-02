#Read parameter value from URL
readFromURL <- function(parameter, session) {
  
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query[[parameter]])){
    
    value <- query[[parameter]]
    return(value)
  
  } 
  
  return(NULL)
  
}