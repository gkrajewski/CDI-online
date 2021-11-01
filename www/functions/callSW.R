callSW <- function(idx, form, lang, done, score){
  
  swCaller <- tryCatch(
    
    expr = {
      
      ### LOGIN TO FIREBASE ###
      
      httr::set_config(httr::config(http_version = 1))
      
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
        
        ### MAKE CALL TO STARWORDS APP ###
        
        body <- content(response, "parsed")
        idToken <- body$idToken
        url <- "https://apps.multilada.pl/starwordsdev/graphql"
        
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
        
        if(length(parsedResp$errors) > 0) return(list(FALSE, parsedResp$errors[[1]]$message))
        return(list(TRUE, ""))
        
      } else {
        
        return(list(FALSE, "ERROR: Cannot connect to Firebase"))
        
      }
      
    },
    
    error = function(e) {
      logerror(paste0(idx, " PINGING SW FAILED! ", e))
      sendMail(subjectText=paste0("[SHINYERROR] ",lang, "-", form, "-", idx,  " CANNOT MAKE CALL"),
               txt=paste0("done: ", done, "\nform: ", form, "\nid: ", idx, "\nscore: ", score, "\n\n", e),
               id=paste0("id=", idx, " form=", form, " lang=", lang),
               host="smtp.gmail.com",
               port=465,
               username=MAIL_USERNAME,
               password=Sys.getenv("GMAIL_PASSWORD"),
               recipients=EMAILS_RECIPIENTS
      )
    }
    
  )
  
}