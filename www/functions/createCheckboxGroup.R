#Create page with multiple checkboxes, title and comment field
createCheckboxGroup <- function(texts, currCat, selected, items, comment){
  
  return(
    
    list(
      
      h4(texts[texts$text_type == currCat, "text"]),
      
      br(),
      
      div(
        
        class = "itemsContainer",
        
        checkboxGroupInput(
          "items",
          selected = selected,
          label = NULL,
          choiceNames = as.character(items[items$category == currCat, ]$definition),
          choiceValues = as.character(items[items$category == currCat, ]$item_id)
        )
        
      ),
      
      div(
        class = "comment",
        textAreaInput("comment", label = texts[texts$text_type == "comment", "text"], value = comment)
      )
      
      
    )   
    
  )
  
}