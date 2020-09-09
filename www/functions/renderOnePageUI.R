#Read answers from file if they exists or prepare empty df      
readAnswers <- function(userAnswersFile){
  
  if (file.exists(userAnswersFile)){
    return(read.csv(userAnswersFile, encoding = "UTF-8"))
  } else {
    return(data.frame(answer = NA))
  }
  
}


#Save data
saveData <- function(input, userAnswersFile, inputID){

  answers <- readAnswers(userAnswersFile)
  answers$answer <- input[[inputID]]
  write.csv(answers, userAnswersFile, row.names = F)

}


#Render main panel
renderMain <- function(input, userAnswersFile, items, texts, output){
  
  #Prepare answers df
  answers <- readAnswers(userAnswersFile)
  
  #Prepare input ID
  inputID <- items$item_id
  
  #Add button service
  observeEvent(input$submit, {
    
    saveData(input, userAnswersFile, inputID)
    output$info <- renderText({texts[texts$text_type == "info", "text"]})
    
  })
  
  #Add no info text when answer is changed
  observeEvent(input[[inputID]], {
    output$info <- renderText({})
  })
  
  if(!is.na(answers$answer)){
    selected <- answers$answer
  } else {
    selected <- character(0)
  }
  
  return(
    
    renderUI({
        
      fluidRow(
        
        column(
          8,
          align = "left",
          items$definition
        ),
        
        column(
          4,
          align = "center",
          radioButtons(
            inputID,
            label = NULL,
            selected = selected,
            choiceNames = list(
              texts[texts$text_type == "option1", "text"],
              texts[texts$text_type == "option2", "text"],
              texts[texts$text_type == "option3", "text"]
            ),
            choiceValues = list("n", "s", "o"),
            inline = T
          )
        )
        
      )
      
    })#end renderUI
    
  )#end return
  
}


#Render UI with one page
renderOnePageUI <- function(input, userAnswersFile, items, texts, output) {
  
  #Render sidebar
  output$sidebar <- renderUI({
    
    list(
      
      p(class = "help-block", texts[texts$text_type == "help", "text"]),
      actionButton("submit", label = texts[texts$text_type == "submit", "text"])
      
    )
    
  })
  
  #Render main
  output$main <- renderMain(input, userAnswersFile, items, texts, output)
  
}