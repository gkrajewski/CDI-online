mergeTranslations <- function(df1, df2, type = "static"){
  
  #Merge two translations files, but the first one is more important than the second
  
  errorMsg <- "There is problem with merging translations files. Investigate the translations and uniTranslations files for the current form <br><br>"
  success <- tryCatch(
    
    expr = {
      
      if (type == "static"){
        df1Str <- paste(df1$text_type, df1$item_type, df1$category)
        df2Str <- paste(df2$text_type, df2$item_type, df2$category)
      } else {
        df1Str <- paste(df1$text_type)
        df2Str <- paste(df2$text_type)
      }
      
      df2 <- subset(df2, !(df2Str %in% df1Str)) #Get things from df2 that are not in df1
      
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
  
  if (success) return(rbind(df1, df2)) else return(NULL)
  
}

