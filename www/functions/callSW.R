# library(jsonlite)
callSW <- function(done = "false"){
  
  ### LOGIN TO FIREBASE ###
  
  authEmail <- Sys.getenv("AUTH_EMAIL")
  authPassword <- Sys.getenv("AUTH_PASSWORD")
  apiKey <- Sys.getenv("API_KEY")
  url <- paste0("https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=", apiKey)
  
  response <-
    POST(
      url,
      body = list(
        email = authEmail,
        password = authPassword,
        returnSecureToken = TRUE
      ),
      encode = "json"
    )
  
  status <- http_status(response)$category
  
  if (status == "Success"){
    
    score <- "false"
    if (done == "true"){
      if (countScore() <= readValueFromNorms()) score <- "true"
    }
    
    ### MAKE CALL TO STARWORDS APP ###
    
    body <- content(response, "parsed")
    idToken <- body$idToken
    url <- "https://apps.multilada.pl/graphql"
    
    body <- paste0("
          mutation{
            upsertCdi(input:{
              token:\"", idToken, "\",
              cdi:{
                done:", done, ",
                cdiTypeId:\"", form, "\",
                childHash:\"", id, "\",
                score: ", score, "}}
                ){
                cdi{
                  pk,
                  cdiTypeId,
                  childHash,
                  score
                }
            }
        }
  ")
    
    response <- POST(url, body = body, accept_json(), content_type("application/graphql"), verbose(data_in = TRUE))
    parsedResp <- content(response, "parsed")
    print(parsedResp)
    
    if(length(parsedResp$errors) > 0) return(FALSE)
    return(TRUE)
    
  } else {
    
    print("ERROR: Cannot connect to Firebase")
    return(FALSE)
    
  }
  
}