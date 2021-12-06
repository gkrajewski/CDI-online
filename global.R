#Load libraries
library(shinythemes)
library(shinyjs)
library(httr) #http requests
library(lubridate) #dates
library(fresh)
library(shinydisconnect) #handling disconnects in a nice visual way
library(emayili) #sending mails
library(RMariaDB) #connecting with MySQL database
library(dplyr) #preparing output file
library(tidyr) #preparing output file
library(logging)
library(mirtCAT)
library(tidyverse) #add_row method
options(stringsAsFactors = FALSE)

#Specify paths
WWW_PATH <- paste0(getwd(),"/www")
FUNCTIONS_PATH <- paste0(WWW_PATH,"/functions")
LANGUAGES_PATH <- paste0(WWW_PATH,"/languages")
INIT_PATH <- getwd()

#Create dirs to saving answers and users' progress
if(!dir.exists(file.path(INIT_PATH, "answers"))) dir.create(file.path(INIT_PATH, "answers"))
if(!dir.exists(file.path(INIT_PATH, "usersProgress"))) dir.create(file.path(INIT_PATH, "usersProgress"))
if(!dir.exists(file.path(INIT_PATH, "CATdesigns"))) dir.create(file.path(INIT_PATH, "CATdesigns"))
if(!dir.exists(file.path(INIT_PATH, "CATsubjects"))) dir.create(file.path(INIT_PATH, "CATsubjects"))
if(!dir.exists(file.path(INIT_PATH, "logs"))) dir.create(file.path(INIT_PATH, "logs"))

#Load functions
source(paste0(FUNCTIONS_PATH,"/readFromURL.R"))
source(paste0(FUNCTIONS_PATH,"/getWholeURL.R"))
source(paste0(FUNCTIONS_PATH,"/renderType.R"))
source(paste0(FUNCTIONS_PATH,"/renderCategory.R"))
source(paste0(FUNCTIONS_PATH,"/createRadioQuestion.R"))
source(paste0(FUNCTIONS_PATH,"/createCheckboxQuestion.R"))
source(paste0(FUNCTIONS_PATH,"/readNorms.R"))
source(paste0(FUNCTIONS_PATH,"/recurrentCallSW.R"))
source(paste0(FUNCTIONS_PATH,"/callSW.R"))
source(paste0(FUNCTIONS_PATH,"/countScore.R"))
source(paste0(FUNCTIONS_PATH,"/prepareOutputStatic.R"))
source(paste0(FUNCTIONS_PATH,"/prepareOutputAdaptative.R"))
source(paste0(FUNCTIONS_PATH,"/runStatic.R"))
source(paste0(FUNCTIONS_PATH,"/runAdaptive.R"))
source(paste0(FUNCTIONS_PATH,"/startTest.R"))
source(paste0(FUNCTIONS_PATH,"/createProgressBar.R"))
source(paste0(FUNCTIONS_PATH,"/sendMail.R"))
source(paste0(FUNCTIONS_PATH,"/sendDatabase.R"))
source(paste0(FUNCTIONS_PATH,"/chooseTheta.R"))
source(paste0(FUNCTIONS_PATH,"/sendLogs.R"))
source(paste0(FUNCTIONS_PATH,"/prepareGroup.R"))
source(paste0(FUNCTIONS_PATH,"/saveCAT.R"))
source(paste0(FUNCTIONS_PATH,"/renderTestingUI.R"))

#Load file with secret variables
readRenviron(".Renviron")

#Set mail things
MAIL_USERNAME <- "cdishiny@gmail.com"
EMAILS_RECIPIENTS <- c("cdishiny@gmail.com", "projekt.starwords@psych.uw.edu.pl")

#Set parameters of saving results in database
STRING_LIMIT <- 2000

#Prepare vector of busy urls and urls to close (to not to allow opening the same inventory more than once)
BUSY_URLS <- reactiveVal(list())
URLS_TO_CLOSE <- reactiveVal(list())

#Prepare logger
basicConfig()
formatter.shiny <- function(record) {
  text <- paste(record$timestamp, record$levelname, record$msg, sep='%')
  return(text)
}
addHandler(writeToFile, file=paste0(INIT_PATH, "/logs/shinyapp.log"), level='DEBUG', 
           formatter=formatter.shiny)

#Create logging table
tableName="logging"
createLogging = paste0("CREATE TABLE `", Sys.getenv("DB_NAME"), "`.`",tableName,"` (
                            `date` DATETIME NULL,
                            `dateString` VARCHAR(50) NULL,
                            `level` VARCHAR(50) NULL,
                            `text` VARCHAR(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL,
                            `id` VARCHAR(99) NULL,
                            `lang` VARCHAR(50) NULL,
                            `form` VARCHAR(50) NULL);
                        ")
sendDatabase(username=Sys.getenv("DB_USERNAME"),
                             password=Sys.getenv("DB_PASSWORD"),
                             dbname=Sys.getenv("DB_NAME"),
                             host=Sys.getenv("DB_HOST"),
                             port=Sys.getenv("DB_PORT"),
                             id="global",
                             tableName=tableName,
                             tableCreate=createLogging)

createEvent = paste0("CREATE EVENT IF NOT EXISTS `", Sys.getenv("DB_NAME"), "`.`Delete_Older_Than_30_Days`
                            ON SCHEDULE EVERY 1 DAY
                            STARTS STR_TO_DATE(DATE_FORMAT(NOW(),'%Y%m%d 0100'),'%Y%m%d %H%i')
                            DO
                            DELETE FROM `", Sys.getenv("DB_NAME"), "`.`",tableName,"`
                            WHERE date < DATE_SUB(NOW(),INTERVAL 30 DAY);
                        ")
sendDatabase(username=Sys.getenv("DB_USERNAME"),
                             password=Sys.getenv("DB_PASSWORD"),
                             dbname=Sys.getenv("DB_NAME"),
                             host=Sys.getenv("DB_HOST"),
                             port=Sys.getenv("DB_PORT"),
                             id="global",
                             tableName=tableName,
                             tableQuery=createEvent)
