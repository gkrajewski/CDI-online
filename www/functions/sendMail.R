sendMail <- function(subjectText, txt, id, host, port, username, password, recipients, attach=NULL){
  
  emailSender <- tryCatch(
    
    expr = {
      
      email <- envelope() %>%
        from(username) %>%
        to(recipients) %>%
        subject(subjectText) %>%
        text(txt)
      
      if (!is.null(attach)) {
        email <- email %>%
          attachment(c(attach))
      }
      
      smtp <- emayili::server(host = host,
                              port = port,
                              username = username,
                              password = password)
      
      smtp(email, verbose = TRUE)
      
      loginfo(paste0(id, ". EMAIL sent"))
    },
    error = function(e) {
      logerror(paste0(id, " EMAIL SAVING FAILED! ", e))
    }
  )
}