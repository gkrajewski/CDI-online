#Create a row with question text on the left and radio buttons on the right
createRadioQuestion <- function(question, inputID, selected, choiceNames, choiceValues){
  
  return(
  
    fluidRow(
    
      column(
        
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