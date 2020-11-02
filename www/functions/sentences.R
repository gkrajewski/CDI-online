#Return fields for writing sentences (takes number of sentences from settings file and current answer - sentences already written)
sentences <- function(){
  
  sentencesNr <- pageSettings$sentences_nr
  
  #Prepare sentences list
  if (is.na(pageAnswer)) sentences <- c(rep("%", sentencesNr))   
  sentences <- strsplit(pageAnswer, "%")[[1]]
  
  #Prepare text input object lists
  textInputObjs <- list()
  for (i in 1 : sentencesNr){
    textInputObjs[[i]] <- textInput(paste0("s", i), label = NULL, value = sentences[i])
  }
  
  return(textInputObjs)
  
}