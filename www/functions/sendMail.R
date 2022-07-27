sendMail <- function(subject, body, id, host="smtp.sendgrid.net", port = 465, username = "cdishiny@onet.pl", recipients = c("cdishiny@onet.pl", "projekt.starwords@psych.uw.edu.pl"), attach=NULL){
  
  tryCatch(
    
    expr = {
      
      mail() |>
        from("cdishiny@onet.pl", "CDI Shiny") |>
        to("cdishiny@onet.pl", "") |>
        subject(subject) |>
        body(body)  |>
        attachments(attach) |>
        send()
      
      loginfo(paste0(id, ". EMAIL sent"))
      
    },
    
    error = function(e) {
      logerror(paste0(id, " EMAIL SENDING FAILED! ", e))
    }
    
  )
  
}