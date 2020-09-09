#Create progress bar depending on current page and page numbers
createProgressBar <- function(currPage, pageNr){
  
  percent <- paste0(as.character(round(currPage/pageNr*100)),"%")
  return(div(class="progress", div(class="progress-bar", style=paste0("width: ",percent), percent)))
  
}


#Read answers from file if they exists or prepare empty df      
readAnswers <- function(userAnswersFile, categories){
  
  if (file.exists(userAnswersFile)){
    return(read.csv(userAnswersFile, encoding = "UTF-8"))
  } else {
    return(data.frame(category = categories, items_selected = NA, comment = NA))
  }
  
}


#Save data
saveData <- function(input, userAnswersFile, currCat, categories){
  
  answers <- readAnswers(userAnswersFile, categories)
  answers[answers$category == currCat,"items_selected"] <- paste(input$items, collapse =  " ")
  answers[answers$category == currCat,"comment"] <- input$comment
  write.csv(answers, userAnswersFile, row.names = F)
  
}


#Render main panel
renderMain <- function(input, userAnswersFile, items, texts, output){
  
  currPage <- reactiveVal(0)
  categories <- unique(items$category)
  pageNr <- length(categories) + 1
  
  #Add next button service
  observeEvent(input$nextB, {
    
    nextPage <- currPage() + 1
    if (nextPage <= pageNr){
      
      #Save data if on current page are input fields
      if (currPage() != 0) saveData(input, userAnswersFile, categories[currPage()], categories)
      
      #Change page
      currPage(nextPage)
      
    } 
    
  })
  
  #Add previous button service
  observeEvent(input$prevB, {
    
    nextPage <- currPage() - 1
    if (nextPage >= 0 ){
      
      #Save data if on current page are input fields
      if (currPage() != pageNr) saveData(input, userAnswersFile, categories[currPage()], categories)
      
      #Change page
      currPage(nextPage)
      
    } 
    
  })
  
  return(
    
    renderUI({
      
      list(
        
        #Render progress bar
        createProgressBar(currPage(),pageNr),
        
        if (currPage() == 0){
          
          #Render first page instruction
          texts[texts$text_type == "instr", "text"]
          
        } else if (currPage() == pageNr) {
          
          #Render last page thanks
          texts[texts$text_type == "thanks", "text"]
          
        } else {
          
          ### Render input page ###
          
          #Get item category for current page
          currCat <- categories[currPage()]
          
          #Prepare answers df
          answers <- readAnswers(userAnswersFile, categories)
          
          #Prepare answers so they can be used in output
          selectedStr <- answers[answers$category == currCat, "items_selected"]
          comment <- answers[answers$category == currCat, "comment"]
          
          if (!is.na(selectedStr)){
            selected <- strsplit(selectedStr, " ")[[1]]
          } else {
            selected <- NULL
          }
          
          if (is.na(comment)){
            comment <- ""
          }
          
          #Render small header, checkboxes and comment field
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
            
            textAreaInput("comment", label = texts[texts$text_type == "comment", "text"], value = comment)
            
          )#end list
          
        }#end else
        
      )#end list
      
    })
    
  )
  
}


#Render UI with multi pages
renderMultiPageUI <- function(input, userAnswersFile, items, texts, output) {
  
  #Render sidebar
  output$sidebar <- renderUI({
    
    list(
      
      p(class = "help-block", texts[texts$text_type == "help", "text"]),
      actionButton("prevB", label = texts[texts$text_type == "prevB", "text"]),
      actionButton("nextB", label = texts[texts$text_type == "nextB", "text"])
      
    )
    
  })
  
  output$main <- renderMain(input, userAnswersFile, items, texts, output)
  
  # #Add button clicks observers
  # PAGE <<- 0
  # maxPage <- length(categories) + 1
  # 
  # observeEvent(input$nextB, {
  #   
  #   print(INPUT_PAGE)
  #   if (PAGE < maxPage){
  #     
  #     if (INPUT_PAGE) saveData(userAnswersFile, input, categories)
  #     PAGE <<- PAGE + 1
  #     output$main <- renderMain(texts, items, input, maxPage, categories, userAnswersFile)
  #     
  #   }
  #   
  # })
  # 
  # observeEvent(input$prevB, {
  #   
  #   print(INPUT_PAGE)
  #   if (PAGE > 0){
  #     
  #     if (INPUT_PAGE) saveData(userAnswersFile, input, categories)
  #     PAGE <<- PAGE - 1
  #     output$main <- renderMain(texts, items, input, maxPage, categories, userAnswersFile)
  #     
  #   }
  #   
  # })
  # 
  # #Render main panel
  # output$main <- renderMain(texts, items, input, maxPage, categories, userAnswersFile)
  
}