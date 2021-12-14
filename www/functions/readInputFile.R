readInputFile <- function(output, path, fileName){
  
  # -Reads given file from given path-
  #
  # returns list of two values:
  # 1 - boolean (TRUE if read successfully, FALSE if not)
  # 2 - file (if read successfully)
  #
  
  errorMsg <- paste0("There is problem with file <b> ", fileName, " </b> that should be located in <b>", path, "</b><br><br>")
  
  success <- tryCatch(
    
    expr = {
      
      setwd(path)
      file <- read.csv(fileName, encoding = "UTF-8", sep = ";", strip.white = T)
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
  
  output <- list(success = success, file = file)
  return(output)
  
}