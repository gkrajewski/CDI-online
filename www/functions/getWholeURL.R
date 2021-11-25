getWholeURL <- function(session){
  
  #Prepares nice string from URL adress
  
  url = paste0(session$clientData$url_protocol,"//", 
               session$clientData$url_hostname,":",
               session$clientData$url_port,
               session$clientData$url_pathname,
               session$clientData$url_search)
  
  return(url)
  
}