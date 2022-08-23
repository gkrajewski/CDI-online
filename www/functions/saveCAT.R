saveCAT <- function(CATdesign, designFile, subject, subjectFile, groupsToSave, urlString, parameters, form, lang, sendLogs, idx){
  
  #Save design and subject files
  saveRDS(CATdesign, designFile)
  saveRDS(subject, subjectFile)
  
  loginfo(paste0(" Groups to save: ", paste0(groupsToSave, collapse=", ")))
  
  #Save every part
  for (group in groupsToSave) {
    
    #Prepare answers file
    answerFile <- paste0("answers/", urlString, "-", group, ".csv")
    outputTable <- read.csv(answerFile)

    if (parameters[parameters$parameter=="email", "value"] == "yes") {
      
      #Send e-mail
      loginfo(paste0(urlString, "-", group, " sending email."))
      sendMail(subject=paste0("[SHINYDATA] ", urlString, "-", group),
               body="Inventory completed.",
               id=paste0(urlString, " group=", group),
               attach=answerFile
      )
      
    } else {
      
      loginfo(paste0(urlString, " sending emails disabled."))
      
    }
    
    #Send responses to database
    if (parameters[parameters$parameter=="database", "value"] == "yes") {
      tableName <- paste0("form_", form, "_", lang, "_adaptive")
      query = paste0("CREATE TABLE `", Sys.getenv("DB_NAME"), "`.`",tableName,"` (
                            `idx` VARCHAR(45) NULL,
                            `gender` VARCHAR(45) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `birth` VARCHAR(45) NULL,
                            `test` VARCHAR(45) NULL,
                            `start_theta` FLOAT NULL,
                            `filler` VARCHAR(45) NULL,
                            `lang` VARCHAR(45) NULL,
                            `group` VARCHAR(45) NULL,
                            `q_id` INT NOT NULL,
                            `items` VARCHAR(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `answers` INT NULL,
                            `comment` VARCHAR(", toString(STRING_LIMIT), ") CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `theta` FLOAT NULL,
                            `se_theta` FLOAT NULL,
                            `final` INT NULL,
                            `start_date` DATETIME NULL,
                            `end_date` DATETIME NULL);")
      
      sendDatabase(username=Sys.getenv("DB_USERNAME"),
                   password=Sys.getenv("DB_PASSWORD"),
                   dbname=Sys.getenv("DB_NAME"),
                   host=Sys.getenv("DB_HOST"),
                   port=Sys.getenv("DB_PORT"),
                   id=paste0(urlString, " group=", group),
                   tableName=tableName,
                   tableCreate=query,
                   tableInput=outputTable)
    }

  }
  
  #Send logs
  if (sendLogs) {
    sendLogs(urlString, idx, form, lang)
  }
  
}