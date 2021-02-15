loginToFirebase <- function(output){
  
  authEmail <- "shiny_user@starwords.eu"
  authPassword <- "#+2I.5]2-xU$SB/2Ll%2"
  
  response <-
    POST(
      "https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=AIzaSyBFJY_mR8R5TXxmbBr9pcpkIBEAsDlDbV8",
      body = list(
        email = authEmail,
        password = authPassword,
        returnSecureToken = TRUE
      ),
      encode = "json"
    )
  
  status <- http_status(response)$category
  # body <- content(response, "text")
  body <- content(response, "parsed")
  
  # output$warning <- renderText({paste(status, body)})
  print(status)
  print(body)
  print(body$idToken)
  
  # print(status)
  # print(body)
  
}