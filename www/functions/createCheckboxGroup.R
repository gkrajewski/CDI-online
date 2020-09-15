prepItems <- function(selectedStr){
  
  if (!is.na(selectedStr)) return(strsplit(selectedStr, " ")[[1]])
  return(NULL)
  
}

prepComment <- function(comment){
  
  if (is.na(comment)) return("")
  return(comment)
  
}


#Create page with multiple checkboxes, title and comment field
createCheckboxGroup <- function(texts, selected, choiceNames, choiceValues, comment){
  
  return(
    
    list(
      
      br(), br(),
      
      div(

        class = "itemsContainer",

        checkboxGroupInput(
          "items",
          selected = selected,
          label = NULL,
          choiceNames = choiceNames,
          choiceValues = choiceValues
        )

      ),
      
      div(
        class = "comment",
        textAreaInput("comment", label = texts[texts$text_type == "comment", "text"], value = comment)
      )
      
      
    )   
    
  )
  
}