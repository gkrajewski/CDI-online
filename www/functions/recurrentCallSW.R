recurrentCallSW <- function(done = "false", score = "false", i = 1, maxI = 3){
  
  callSuccess <- callSW(done, score)
  
  if (!callSuccess & i < maxI){
    
    i <- i + 1
    delay(3000, recurrentCallSW(done, score, i))
    
  } else if (!callSuccess & i == maxI) {
    
    print("ERROR: Cannot make successfull call")
    
    email <-
      gm_mime() %>%
      gm_to(emailTo) %>%
      gm_from("cdishiny@gmail.com") %>%
      gm_subject("[SHINYERROR] CANNOT MAKE CALL") %>%
      gm_text_body(paste0("done: ", done, "\nform: ", form, "\nid: ", idx, "\nscore: ", score), charset = "utf-8")

    gm_send_message(email)
    
  }
  
}