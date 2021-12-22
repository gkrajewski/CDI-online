readInputFile <- function(output, path, fileName, sep = ","){
  
  #Reads given file from given path. Returns file or NULL if error
  errorMsg <- paste0("There is problem with file <b> ", fileName, " </b> that should be located in <b>", path, "</b><br><br>")
  
  success <- tryCatch(
    
    expr = {
      
      setwd(path)
      file <- read.csv(fileName, encoding = "UTF-8", sep = sep, strip.white = T)
      file[is.na(file)] <- ""
      setwd(INIT_PATH)
      
      TRUE
      
    },
    
    error = function(m){
      
      msg <- paste0(errorMsg, m)
      logerror(msg)
      output$sidebar <- renderText({msg})
      return(FALSE)
      
    },
    
    warning = function(m){
      
      msg <- paste0(errorMsg, m)
      logwarn(msg)
      output$sidebar <- renderText({msg})
      return(FALSE)
      
    }
    
  )
  
  if (success) return(file) else return(NULL)
  
}