renderType <- function(input, output, type, reactList, staticList){

  #Set current type
  reactList$type <- type
  
  #Adjust css of menu buttons and update progress
  for (t in staticList$types) {
    divId <- paste0(t, "container")
    if (t == type){
      reactList$userProgress[reactList$userProgress$type == t, "current"] <- TRUE
      addClass(divId, "menuButtonContainerActive")
    } else {
      reactList$userProgress[reactList$userProgress$type == t, "current"] <- FALSE
      removeClass(divId, "menuButtonContainerActive")
    }
  }

  #Get type items, texts and settings
  reactList$typeItems <- staticList$items[staticList$items$type == type, ]
  reactList$typeTxt <- staticList$txt[staticList$txt$item_type == type, ]
  reactList$typeSettings <- staticList$settings[staticList$settings$type == type, ]
  
  #Render type header
  output$header <- renderText({reactList$typeTxt[reactList$typeTxt$text_type == "header", "text"]})
  
  #Get type categories
  reactList$categories <- setdiff(unique(reactList$typeTxt$category), c("", "allInput"))
  reactList$categoriesNr <- length(reactList$categories)
  
  #Get first and last category if type with more than one category
  if (reactList$categoriesNr > 1){
    reactList$firstCat <- reactList$categories[1]
    ifelse(reactList$firstCat == "", reactList$categories[2], reactList$firstCat)
    reactList$lastCat <- reactList$categories[length(reactList$categories)]
  }
  
  #Needed for correct functioning of observer (sentences data saving)
  reactList$sentencesNr <- 0
  
  #Render category
  reactList <- renderCategory(input, output, reactList$userProgress[reactList$userProgress$type == type, "category"], reactList, staticList)
  
  #Return updated reactiveVal list
  return(reactList)
  
}