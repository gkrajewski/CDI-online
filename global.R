#Load libraries
library(shinythemes) #for design
library(shinyjs) #for nice functions like hiding buttons or adding css classes to objects
library(httr) #for handling HTTP requests
library("lubridate")  #for data manipulation

#Needed for correct functioning
options(stringsAsFactors = FALSE)

#Specify data paths etc.
dataPath <- paste0(getwd(),"/www")
functionsPath <- paste0(dataPath,"/functions")
initPath <- getwd()

#Create folders with user userProgress and answers if they don't exist
if(!dir.exists(file.path(initPath, "answers"))) dir.create(file.path(initPath, "answers")) 
if(!dir.exists(file.path(initPath, "userProgress"))) dir.create(file.path(initPath, "userProgress")) 

#Load universal settings
setwd(dataPath)
uniSettings <<- read.csv("uniSettings.csv", encoding = "UTF-8", strip.white = T)
setwd(initPath)

#Load functions
source(paste0(functionsPath,"/readFromURL.R"))
source(paste0(functionsPath,"/loginToFirebase.R"))
#source(paste0(functionsPath,"/doPOST.R"))
source(paste0(functionsPath,"/addSidebarObservers.R"))
source(paste0(functionsPath,"/badDate.R"))
source(paste0(functionsPath,"/addDataSaving.R"))
source(paste0(functionsPath,"/renderType.R"))
source(paste0(functionsPath,"/renderPage.R"))
source(paste0(functionsPath,"/renderInputObject.R"))
source(paste0(functionsPath,"/countScore.R"))