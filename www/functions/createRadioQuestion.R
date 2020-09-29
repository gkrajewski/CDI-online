#Create a row with question text on the left and radio buttons on the right
createRadioQuestion <- function(question, inputID, selected, choiceNames, choiceValues, session){
  
  if (selected == "e") selected <- character(0)
  clickID <- paste0("clickableArea_", inputID)
  
  observe({
    onclick(clickID, updateRadioButtons(session, inputID, selected = character(0)))
  })

  return(
  
    fluidRow(
    
      column(
        
        class = "radioQuestion",
        id = clickID,
        8,
        align = "left",
        question
        
      ),
      
      column(
        
        4,
        align = "center",
        
        radioButtons(
          
          inputID,
          label = NULL,
          selected = selected,
          choiceNames = choiceNames,
          choiceValues = choiceValues,
          inline = T
        
        )
      
      )#end column
    
    )#end fluidRow    
  
  )#end return
  
}