sendLogs <- function(urlString, idx, form, lang){
  
  tryCatch(
    
    expr = {
      
      logs <- read.table(paste0(INIT_PATH, "/logs/", urlString, ".log"), sep="%", blank.lines.skip = TRUE, quote="", fill = TRUE)
      
      #Copy text from broken rows (no log type) to previous rows
      brokenRows <- which(logs[,2] == '')
      for (row in brokenRows) logs[row - 1,3] <- paste(logs[row - 1, 3], logs[row,1])
      logs <- logs[-brokenRows, ]
      
      colnames(logs) <- c("date", "level", "text")
      logs$date <- as.POSIXct(logs$date)
      date_max <- max(logs[grepl("session started", logs$text, fixed=T), "date"])
      new_logs <- logs[logs$date>=date_max, ]
      new_logs$dateString <- as.character(new_logs$date)
      new_logs$id <- idx
      new_logs$form <- form
      new_logs$lang <- lang
      sendDatabase(username=Sys.getenv("DB_USERNAME"), 
                   password=Sys.getenv("DB_PASSWORD"),
                   dbname=Sys.getenv("DB_NAME"), 
                   host=Sys.getenv("DB_HOST"), 
                   port=Sys.getenv("DB_PORT"), 
                   id=urlString, 
                   tableName="logging", 
                   tableInput=new_logs)
      
    },
    
    error = function(m){
      msg <- paste0("LOGS SENDING FAILED!", m)
      logerror(msg)
    }
    
  )
  
}