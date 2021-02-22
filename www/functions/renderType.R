renderType <- function(input, output, type){
  
  currType <<- type

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
  
  typeTxt <<- txt[txt$item_type == currType,]
  typeItems <<- items[items$type == currType,]
  typeSettings <<- settings[settings$type == currType, ]

  output$header <- renderText({typeTxt[typeTxt$text_type == "header", "text"]})
  output$warning <- renderText({})
  
  currCat <<- userProgress[userProgress$type == currType, "category"]

  renderCategory(input, output)
  
}