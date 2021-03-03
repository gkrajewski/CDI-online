recurrentCallSW <- function(done = "false", score = "false", i = 1, maxI = 3){
  
  callSuccess <- callSW(done, score)
  
  if (!callSuccess & i < maxI){
    
    i <- i + 1
    delay(3000, recurrentCallSW(done, score, i))
    
  } else if (!callSuccess & i == maxI) {
    
    print("ERROR: Cannot make successfull call")
    
    send.mail(
      from = "cdishiny@gmail.com",
      to = emailTo,
      subject = "[SHINYERROR] CANNOT MAKE CALL",
      body = paste0("done: ", done, "\nform: ", form, "\nid: ", idx, "\nscore: ", score),
      smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "cdishiny@gmail.com", passwd = Sys.getenv("GMAIL_PASSWORD"), ssl = TRUE),
      authenticate = TRUE,
      send = TRUE,
    )
    
  }
  
}