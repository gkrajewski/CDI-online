readValueFromNorms <- function(percentile = "p_0.1"){
  
  ### Demographic answer and norms are needed for this function to work ###
  
  if (!is.null(norms)){
    
    if (!is.na(answers[answers$answer_type == "demographic", "answer"])){
      
      demoAnswer <- answers[answers$answer_type == "demographic", "answer"]
      demoAnswer <- strsplit(demoAnswer, ",")[[1]]
      birthDate <- demoAnswer[1]
      
      age <- interval(birthDate, Sys.Date()) %/% months(1)
      print(paste0("Wiek: ", age))
      ageStr <- paste0("m_", age)
      
      return(norms[ageStr, percentile])
      
    } else {
      
      print("ERROR: No birth date for readValueFromNorms function")
      
    }    
    
  } else {
    
    print("ERROR: No norms loaded")
    
  }
  
}