badDate <- function(norms, birthDate){
  
  age <- interval(birthDate, Sys.Date()) %/% months(1)
  monthsInNorms <- lapply(row.names(norms), function(i){as.numeric(substr(i, 3, nchar(i)))})
  return(!is.element(age, monthsInNorms))
  
}