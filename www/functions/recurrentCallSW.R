recurrentCallSW <- function(idx, form, done = "false", score = "false", i = 1, maxI = 3){
  
  callSuccess <- callSW(idx, form, done, score)
  
  if (!callSuccess & i < maxI){
    
    i <- i + 1
    delay(3000, recurrentCallSW(idx, form, done, score, i))
    
  } else if (!callSuccess & i == maxI) {
    
    print("ERROR: Cannot make successfull call")
    
    send.mail(
      from = MAIL_USERNAME,
      to = EMAILS_RECIPIENTS,
      subject = "[SHINYERROR] CANNOT MAKE CALL",
      body = paste0("done: ", done, "\nform: ", form, "\nid: ", idx, "\nscore: ", score),
      smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = MAIL_USERNAME, passwd = Sys.getenv("GMAIL_PASSWORD"), ssl = TRUE),
      authenticate = TRUE,
      send = TRUE,
    )
    
  }
  
}