library(shinythemes)
library(shinyjs)
library(httr)
library(lubridate)
library(fresh)
library(mailR) #sending mails
library(shinydisconnect) #handling disconnects in a nice visual way
library(RMariaDB) #connecting with MySQL database

options(stringsAsFactors = FALSE)

dataPath <- paste0(getwd(),"/www")
functionsPath <- paste0(dataPath,"/functions")
initPath <- getwd()

if(!dir.exists(file.path(initPath, "answers"))) dir.create(file.path(initPath, "answers"))
if(!dir.exists(file.path(initPath, "usersProgress"))) dir.create(file.path(initPath, "usersProgress"))

setwd(dataPath)
endSettings <<- read.csv("endSettings.csv", encoding = "UTF-8", strip.white = T)
setwd(initPath)

source(paste0(functionsPath,"/readFromURL.R"))
source(paste0(functionsPath,"/startApp.R"))
source(paste0(functionsPath,"/readNormsFile.R"))
source(paste0(functionsPath,"/readValueFromNorms.R"))
source(paste0(functionsPath,"/recurrentCallSW.R"))
source(paste0(functionsPath,"/callSW.R"))
source(paste0(functionsPath,"/addSidebarObservers.R"))
source(paste0(functionsPath,"/addDataSaving.R"))
source(paste0(functionsPath,"/renderType.R"))
source(paste0(functionsPath,"/renderCategory.R"))
source(paste0(functionsPath,"/renderInput.R"))
source(paste0(functionsPath,"/badDate.R"))
source(paste0(functionsPath,"/countScore.R"))


#Load file with secret variables
readRenviron("Renviron")

#Set mail things
MAIL_USERNAME = "cdishiny@gmail.com"
EMAILS_RECIPIENTS <- c("cdishiny@gmail.com", "projekt.starwords@psych.uw.edu.pl")
DB_USERNAME = "root"
DB_HOST = '127.0.0.1'
DB_PORT = "3306"
DB_NAME = "shinyapp"


#Prepare vector of busy urls and urls to close (to not allow few opens of the same inventory)
BUSY_URLS <- reactiveVal(list())
URLS_TO_CLOSE <- reactiveVal(list())
