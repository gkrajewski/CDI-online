#Create progress bar depending on current page and page numbers
createProgressBar <- function(currPage, pageNr){
  
  percent <- paste0(as.character(round(currPage/pageNr*100)),"%")
  return(div(class="progress", div(class="progress-bar", style=paste0("width: ",percent), percent)))
  
}