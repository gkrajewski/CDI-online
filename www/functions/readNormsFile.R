readNormsFile <- function(gender = NULL){
  
  ### If no gender is given unisex norms are taken (default) ###
  ### If form is wg comp norms are taken ###
  
  if (!dir.exists(file.path(formPath, "norms"))){
    
    return(NULL)
    
  } else {
    
    if (form == "wg"){
      
      comp = TRUE
      
    } else {
      
      comp = FALSE
      
    }
    
    if (is.null(gender)){
      
      unisex = TRUE
      
    } else {
      
      unisex = FALSE
      
    }
    
    if (unisex){
      
      if (comp){
        
        normsFile <- "unisex_comp.csv"
        
      } else {
        
        normsFile <- "unisex.csv"
        
      }
      
    } else {
      
      if (comp){
        
        normsFile <- paste0(gender, "_comp.csv")
        
      } else {
        
        normsFile <- paste0(gender, ".csv")
        
      }
      
    }
    
    setwd(paste0(formPath, "/norms"))
    
    if (file.exists(normsFile)){
      
      print(normsFile)
      return(read.csv(normsFile, encoding = "UTF-8", strip.white = T, row.names = 1))
      
    } else {
      
      print(paste0("ERROR: Cannot find ", normsFile, " file"))
      return(NULL)
      
    }
    
    setwd(initPath)
    
  }
  
}