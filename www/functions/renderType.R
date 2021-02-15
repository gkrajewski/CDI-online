renderType <- function(input, output, type){
  
  #Set global variable 'currType' value to parameter 'type' value
  currType <<- type
  
  #Get current category for current type
  currCat <<- userProgress[userProgress$type == currType, "category"]

  #Set proper type button as current (in userProgress df and proper design)
  for (type in types) {
    
    divId <- paste0(type, "container")
  
    if (type == currType){
      
      userProgress[userProgress$type == type, "current"] <<- TRUE
      addClass(divId, "menuButtonContainerActive")
      
    } else {
      
      userProgress[userProgress$type == type, "current"] <<- FALSE
      removeClass(divId, "menuButtonContainerActive")
      
    }
    
  }
  
  #Get translations, items and settings connected with type
  typeTxt <<- txt[txt$item_type == currType,]
  typeItems <<- items[items$type == currType,]
  typeSettings <<- settings[settings$type == currType, ]

  #Render header
  output$header <- renderText({typeTxt[typeTxt$text_type == "header", "text"]})
  
  #Render page (rest of UI)
  renderPage(input, output)
  
}