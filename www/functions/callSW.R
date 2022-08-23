callSW <- function(idx, form, lang, done, score){ 
 
  swCaller <- tryCatch(
    
    expr = {
      
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
        
        loginfo(paste0(lang, "-", form, "-", idx, " successfully connected to SW database"))
        
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
                childHash:\"", idx, "\",
                language:\"", lang, "\",
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
        
        if(length(parsedResp$errors) > 0) {
      
          logerror(paste0(form, "-", lang, "-", idx, " ERROR: Cannot update the SW database"))
          return(list(FALSE, parsedResp$errors[[1]]$message))
    
        } 
    
        loginfo(paste0(form, "-", lang, "-", idx, " successfully updating status in SW database"))
        return(list(TRUE, ""))
    
      } else {
        
        logerror(paste0(form, "-", lang, "-", idx, " ERROR: Cannot connect to SW database"))
        return(list(FALSE, "ERROR: Cannot connect to SW database"))
            
      }
      
    },
    
    error = function(e) {
      logerror(paste0(idx, " PINGING SW FAILED! ", e))
      sendMail(subject=paste0("[SHINYERROR] ",lang, "-", form, "-", idx,  " CANNOT MAKE CALL"),
               body=paste0("done: ", done, "\nform: ", form, "\nid: ", idx, "\nscore: ", score, "\n\n", e),
               id=paste0("id=", idx, " form=", form, " lang=", lang)
      )
    }
    
  )
  
}