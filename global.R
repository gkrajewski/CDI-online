#Load libraries
library(shinythemes) #for design
library(shinyjs) #for nice functions like hiding buttons or adding css classes to objects

#Needed for correct functioning
options(stringsAsFactors = FALSE)

#Specify data paths etc.
dataPath <- paste0(getwd(),"/www")
functionsPath <- paste0(dataPath,"/functions")
initPath <- getwd()

#Create folders with user progress and answers if they don't exist
ifelse(!dir.exists(file.path(initPath, "answers")), dir.create(file.path(initPath, "answers")), FALSE)
ifelse(!dir.exists(file.path(initPath, "progress")), dir.create(file.path(initPath, "progress")), FALSE)

#Load universal translations and settings
setwd(dataPath)
uniSettings <<- read.csv("settings.csv", encoding = "UTF-8", strip.white = T)
setwd(initPath)

#Load functions
source(paste0(functionsPath,"/readFromURL.R"))
source(paste0(functionsPath,"/addSidebarObservers.R"))
source(paste0(functionsPath,"/addDataSaving.R"))
source(paste0(functionsPath,"/renderType.R"))
source(paste0(functionsPath,"/renderPage.R"))
source(paste0(functionsPath,"/renderInputObject.R"))
source(paste0(functionsPath,"/countScore.R"))