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
if (!dir.exists(file.path(INIT_PATH, "designs"))) dir.create(file.path(INIT_PATH, "designs"))
if (!dir.exists(file.path(INIT_PATH, "subjects"))) dir.create(file.path(INIT_PATH, "subjects"))

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
source(paste0(FUNCTIONS_PATH,"/prepareOutputStatic.R"))
source(paste0(FUNCTIONS_PATH,"/prepareOutputAdaptative.R"))
source(paste0(FUNCTIONS_PATH,"/runStatic.R"))
source(paste0(FUNCTIONS_PATH,"/runAdaptive.R"))
source(paste0(FUNCTIONS_PATH,"/startTest.R"))
source(paste0(FUNCTIONS_PATH,"/createProgressBar.R"))

#Load file with secret variables
readRenviron(".Renviron")

#Set mail things
MAIL_USERNAME <- "cdishiny@gmail.com"
EMAILS_RECIPIENTS <- c("cdishiny@gmail.com", "projekt.starwords@psych.uw.edu.pl")

# credentials to connect with database
STRING_LIMIT <- 2000

#Prepare vector of busy urls and urls to close (to not allow few opens of the same inventory)
BUSY_URLS <- reactiveVal(list())
URLS_TO_CLOSE <- reactiveVal(list())

#prepare logger
basicConfig()