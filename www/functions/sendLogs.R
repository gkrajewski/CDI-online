sendLogs <- function(urlString, idx, form, lang){
  
  logs <- read.table(paste0(INIT_PATH, "/logs/", urlString, ".log"), sep="%", blank.lines.skip = TRUE, quote="")
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
}