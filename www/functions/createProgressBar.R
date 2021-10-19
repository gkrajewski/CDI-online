createProgressBar <- function(output, CATdesign, maxItemNr){
  
  #Get number of already answered items
  itemsAnsweredNr <- length(na.omit(CATdesign$person$items_answered))
  
  #Prepare fraction
  fraction <- itemsAnsweredNr / maxItemNr
  
  #Render progress bar
  output$progressBar <- renderUI({
    div(
      class = "progress",
      div(
        class = "progress-bar",
        style = paste0("width: ", ceiling(100*fraction), "%;"),
        paste0(ceiling(100*fraction), "%")
      )
    )
  })
  
}