readFromURL <- function(parameter, session, caseInvariance = TRUE) {
  
  #Reads given parameter value from URL query
  
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query[[parameter]])){
    value <- query[[parameter]]
    if (caseInvariance){
      return(tolower(value))
    } else {
      return(value)
    }
  } 
  
  return(NULL)
  
}