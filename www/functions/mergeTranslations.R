mergeTranslations <- function(df1, df2){
  
  #Merge two translations data frames but the first df is more important than the second
  df3 = merge(df1, df2, by = setdiff(intersect(names(df1), names(df2)), c("text")), all= TRUE, sort = FALSE)
  df3$text.x[is.na(df3$text.x)] <- df3$text.y[is.na(df3$text.x)]
  df3$text.y <- NULL
  names(df3)[names(df3) == 'text.x'] <- 'text'
  return(df3)
  
}

