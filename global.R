#Load libraries
library(shinythemes)
library(shinyjs)
library(httr) #http requests
library(lubridate) #dates
library(fresh)
library(mailR) #sending mails
options(stringsAsFactors = FALSE)

#Specify paths
wwwPath <- paste0(getwd(),"/www")
functionsPath <- paste0(wwwPath,"/functions")
languagesPath <- paste0(wwwPath,"/languages")
initPath <- getwd()

#Create dirs to saving answers and users' progress
if(!dir.exists(file.path(initPath, "answers"))) dir.create(file.path(initPath, "answers"))
if(!dir.exists(file.path(initPath, "usersProgress"))) dir.create(file.path(initPath, "usersProgress"))

#Load form-universal end settings
setwd(wwwPath)
endSettings <<- read.csv("endSettings.csv", encoding = "UTF-8", strip.white = T)
setwd(initPath)

#Load functions
source(paste0(functionsPath,"/readFromURL.R"))
source(paste0(functionsPath,"/startInventory.R"))
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
readRenviron(".Renviron")

#Specify recipients of mails produced by app
emailTo <<- c("cdishiny@gmail.com", "projekt.starwords@psych.uw.edu.pl")