sendMail <- function(subject, body, id, host="smtp.sendgrid.net", port = 465, sender = "cdishiny@gmail.com", recipient = "projekt.starwords@psych.uw.edu.pl", attach=NULL){
  
  tryCatch(
    
    expr = {
      
      mail() |>
        from(sender, "CDI Shiny") |>
        to(recipient, "") |>
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