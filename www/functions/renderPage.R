renderPage <- function(input, output){
  
  #Update progress df
  progress[progress$type == currType, "category"] <<- currCat
  
  #Get number of categories for current type
  categoriesNum <- length(unique(typeItems$category))
  
  if (categoriesNum > 1){
    
    #Get items categories
    categories <<- unique(typeItems$category)
    
    #Get last category
    lastCat <- categories[length(categories)]
    
    #Adjust for multi page design
    if (currCat == "none"){
      
      pageType <- "first"
      pageNr <- 1
      pageTxt <<- typeTxt[typeTxt$category == currCat, ]
      output$buttons <- renderUI({actionButton("nextBtn", label = txt[txt$text_type == "nextBtn", "text"])})
      
    } else {
      
      pos <- match(currCat, categories)
      pageNr <- pos + 1
      
      if (currCat == lastCat){
        
        pageType <- "last"
        pageItems <<- typeItems[typeItems$category == currCat, ]
        pageTxt <<- typeTxt[typeTxt$category == currCat | typeTxt$category == "allInput", ]
        output$buttons <- renderUI({
          list(
            actionButton("backBtn", label = txt[txt$text_type == "backBtn", "text"]),
            actionButton("saveBtn", label = txt[txt$text_type == "saveBtn", "text"])
          )
        })
        
      } else {
        
        pageType <- "input"
        pageItems <<- typeItems[typeItems$category == currCat, ]
        pageTxt <<- typeTxt[typeTxt$category == currCat | typeTxt$category == "allInput", ]
        output$buttons <- renderUI({
          list(
            actionButton("backBtn", label = txt[txt$text_type == "backBtn", "text"]),
            actionButton("nextBtn", label = txt[txt$text_type == "nextBtn", "text"])
          )
        })
        
      }
      
    }
    
    #Render page number
    output$page <- renderText({paste0(as.character(pageNr), "/", as.character(categoriesNum + 1))})
    
  } else {
    
    #One page type
    pageType <<- "input&save"
    pageItems <<- typeItems
    pageTxt <<- typeTxt
    if (currType != "end"){
      output$buttons <- renderUI({actionButton("saveBtn", label = txt[txt$text_type == "saveBtn", "text"])})
    } else {
      output$buttons <- renderUI({})
    }
    output$page <- renderText({})
    
  }
  
  #Get page settings
  pageSettings <<- typeSettings[typeSettings$page_type == pageType, ]
  
  #Get page input object type
  pageInputType <<- pageSettings$input_type
  
  if (pageInputType != "none"){
    
    #Add proper record to answers df if it doesn't exist
    record <- c(currType, currCat, pageInputType, NA)
    answers <<- rbind(if(!tail(duplicated(rbind(answers[1:3],record[1:3])),1)) record, answers)

    #Get current page answers
    pageAnswer <<- answers[answers$type == currType & answers$category == currCat & answers$answer_type == pageInputType, "answer"]
    
    if (pageSettings$comment){
      
      #Add proper record to answers df if it doesn't exist
      record <- c(currType, currCat, "comment", NA)
      answers <<- rbind(if(!tail(duplicated(rbind(answers[1:3],record[1:3])),1)) record, answers)
      
      #Get current page comment value
      commentValue <- answers[answers$type == currType & answers$category == currCat & answers$answer_type == "comment", "answer"]
      if (is.na(commentValue)) commentValue <- ""
      
    }     
    
  }
  
  #Render help block text (if any)
  if (pageSettings$help){
    output$help <- renderText({pageTxt[pageTxt$text_type == "help", "text"]})
  } else {
    output$help <- renderText({})
  }
  
  #Render main container according to page settings
  output$main <- renderUI({
    
    list(
      
      #Render header of category
      if (pageSettings$cat_header) h4(pageTxt[pageTxt$text_type == "catHeader", "text"]),
      
      #Render long paragraph
      if (pageSettings$long_text) p(pageTxt[pageTxt$text_type == "longText", "text"]),
      
      #Render short instruction
      if (pageSettings$instr) h5(pageTxt[pageTxt$text_type == "instr", "text"]),
      
      #Render warning message
      if (pageSettings$warning) p(strong(pageTxt[pageTxt$text_type == "warning", "text"])),
      
      #Render input object
      if (pageInputType != "none"){
        
        #inputObject <- do.call(pageInputType, list())
        inputObject <- renderInputObject()
        if (pageSettings$css_class != "none") inputObject <- div(class=pageSettings$css_class, inputObject)
        inputObject
        
      },
      
      #Render comment field
      if (pageSettings$comment){
        
        div(
          class = "comment",
          textAreaInput("comment", label = txt[txt$text_type == "commentLabel", "text"], value = commentValue)
        )
          
      }#end if
      
    )#end list
    
  })#end renderUI
  
}#end renderPage