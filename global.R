options(stringsAsFactors = FALSE)

#Load Polish texts
lang <- "Polish"
wd.datapath = paste0(getwd(),"/www")
wd.init = getwd()
setwd(wd.datapath)
texts = read.csv("texts.csv", encoding = "UTF-8")
texts <- texts[texts$language == lang,]
setwd(wd.init)

