readInputFile <- function(path, fileName, sep = ";"){
 
  setwd(path)
  file <- read.csv(fileName, encoding = "UTF-8", sep = sep, strip.white = T)
  file[is.na(file)] <- ""
  setwd(INIT_PATH)
   
  return(file)
  
}