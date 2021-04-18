#Load libraries
library(shinythemes)
library(shinyjs)
library(httr) #http requests
library(lubridate) #dates
library(fresh)
library(mailR) #sending mails
library(shinydisconnect) #handling disconnects in a nice visual way
library(RMariaDB) #connecting with MySQL database
library(dplyr) #preparing output file
library(tidyr) #preparing output file
options(stringsAsFactors = FALSE)

#Specify paths
WWW_PATH <- paste0(getwd(),"/www")
FUNCTIONS_PATH <- paste0(WWW_PATH,"/functions")
LANGUAGES_PATH <- paste0(WWW_PATH,"/languages")
INIT_PATH <- getwd()

#Create dirs to saving answers and users' progress
if(!dir.exists(file.path(INIT_PATH, "answers"))) dir.create(file.path(INIT_PATH, "answers"))
if(!dir.exists(file.path(INIT_PATH, "usersProgress"))) dir.create(file.path(INIT_PATH, "usersProgress"))

#Load form-universal end settings
setwd(WWW_PATH)
END_SETTINGS <- read.csv("endSettings.csv", encoding = "UTF-8", strip.white = T)
setwd(INIT_PATH)

#Load functions
source(paste0(FUNCTIONS_PATH,"/readFromURL.R"))
source(paste0(FUNCTIONS_PATH,"/renderType.R"))
source(paste0(FUNCTIONS_PATH,"/renderCategory.R"))
source(paste0(FUNCTIONS_PATH,"/createRadioQuestion.R"))
source(paste0(FUNCTIONS_PATH,"/createCheckboxQuestion.R"))
source(paste0(FUNCTIONS_PATH,"/readNorms.R"))
source(paste0(FUNCTIONS_PATH,"/recurrentCallSW.R"))
source(paste0(FUNCTIONS_PATH,"/callSW.R"))
source(paste0(FUNCTIONS_PATH,"/countScore.R"))
source(paste0(FUNCTIONS_PATH,"/prepareOutput.R"))


#Load file with secret variables
readRenviron("Renviron")

#Set mail things
MAIL_USERNAME = "cdishiny@gmail.com"
EMAILS_RECIPIENTS <- c("cdishiny@gmail.com", "projekt.starwords@psych.uw.edu.pl")
#DB_USERNAME = "root"
#DB_HOST = '127.0.0.1'
#DB_PORT = "3306"
#DB_NAME = "shinyapp"

DB_USERNAME = "cdi_vtOgKdtu"
DB_HOST = 'sk201194-001.dbaas.ovh.net'
DB_PORT = "35324"
DB_NAME = "cdi"


#Prepare vector of busy urls and urls to close (to not allow few opens of the same inventory)
BUSY_URLS <- reactiveVal(list())
URLS_TO_CLOSE <- reactiveVal(list())

