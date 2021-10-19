recurrentCallSW <- function(idx, form, lang, done = "false", score = "false", i = 1, maxI = 3){
  
  listReturned <- callSW(idx, form, lang, done, score)
  callSuccess <- listReturned[[1]]
  
  if (!callSuccess & i < maxI){
    
    i <- i + 1
    delay(3000, recurrentCallSW(idx, form, lang, done, score, i))
    
  } else if (!callSuccess & i == maxI) {
    
    print("ERROR: Cannot make successfull call")
    errorMsg <- listReturned[[2]]
    
    sendMail(subjectText="[SHINYERROR] CANNOT MAKE CALL",
             txt=paste0("done: ", done, "\nform: ", form, "\nid: ", idx, "\nscore: ", score, "\n\n", errorMsg),
             id=paste0("id=", idx, " form=", form, " lang=", lang),
             host="smtp.gmail.com",
             port=465,
             username=MAIL_USERNAME,
             password=Sys.getenv("GMAIL_PASSWORD"),
             recipients=EMAILS_RECIPIENTS
    )
    
  }
  
}