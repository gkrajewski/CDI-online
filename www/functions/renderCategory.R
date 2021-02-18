renderCategory <- function(input, output){
  
  userProgress[userProgress$type == currType, "category"] <<- currCat
  specialCategories <- c("", "allInput")
  categories <<- unique(typeTxt$category)
  categories <<- setdiff(categories, specialCategories)
  categoriesNum <- length(categories)
  buttons <- NULL
  currCatNrTxt <- NULL
  
  if (categoriesNum > 1){
    
    firstCat <- categories[1]
    ifelse(firstCat == "", categories[2], categories[1])
    lastCat <- categories[length(categories)]
    catItems <<- typeItems[typeItems$category == currCat, ]
    
    if (is.element(currCat, typeSettings$category)){
      catSettings <<- typeSettings[typeSettings$category == currCat, ]
    } else {
      catSettings <<- typeSettings[typeSettings$category == "" | is.na(typeSettings$category), ]
    }
    
    if (currCat == firstCat){
      
      catTxt <<- typeTxt[typeTxt$category == currCat, ]
      buttons <- list(actionButton("nextBtn", label = txt[txt$text_type == "nextBtn", "text"]))
      
    } else {
      
      catTxt <<- typeTxt[typeTxt$category == currCat | typeTxt$category == "allInput", ]
      
      if (currCat == lastCat){

        buttons <- list(
          actionButton("backBtn", label = txt[txt$text_type == "backBtn", "text"]),
          actionButton("saveBtn", class = "btn-primary", label = txt[txt$text_type == "saveBtn", "text"])
        )
        
      } else {
        
        buttons <- list(
          actionButton("backBtn", label = txt[txt$text_type == "backBtn", "text"]),
          actionButton("nextBtn", label = txt[txt$text_type == "nextBtn", "text"])
        )
        
      }
      
    }
  
    currCatNr <<- match(currCat, categories)
    currCatNrTxt <- paste0(as.character(currCatNr), "/", as.character(categoriesNum))
    
  } else {
    
    catItems <<- typeItems
    catTxt <<- typeTxt
    catSettings <<- typeSettings
    if (currType != "postEnd") buttons <- list(actionButton("saveBtn", class = "btn-primary", label = txt[txt$text_type == "saveBtn", "text"]))
    
  }
  
  inputType <<- catSettings$input_type
  
  if (!is.na(inputType)){
    
    record <- c(currType, currCat, inputType, NA)
    answers <<- rbind(if(!tail(duplicated(rbind(answers[1:3],record[1:3])),1)) record, answers)
    catAnswer <<- answers[answers$type == currType & answers$category == currCat & answers$answer_type == inputType, "answer"]
    
    if (catSettings$comment){
      
      record <- c(currType, currCat, "comment", NA)
      answers <<- rbind(if(!tail(duplicated(rbind(answers[1:3],record[1:3])),1)) record, answers)
      commentValue <- answers[answers$type == currType & answers$category == currCat & answers$answer_type == "comment", "answer"]
      if (is.na(commentValue)) commentValue <- ""
      
    }     
    
  }
  
  output$sidebar <- renderUI({
    
    list(
      if (is.element("help", catTxt$text_type)) div(class="help-block", catTxt[catTxt$text_type == "help", "text"]),
      if (!is.null(currCatNrTxt)) h2(currCatNrTxt),
      if (!is.null(buttons)) div(id="buttons", buttons)
    )
    
  })
  
  notesList <- list()
  
  output$main <- renderUI({
    
    list(
      
      if (is.element("catHeader", catTxt$text_type)) h4(catTxt[catTxt$text_type == "catHeader", "text"]),
      if (is.element("longText", catTxt$text_type)) p(catTxt[catTxt$text_type == "longText", "text"]),
      if (is.element("instr", catTxt$text_type)) h5(catTxt[catTxt$text_type == "instr", "text"]),
      if (is.element("warning", catTxt$text_type)) p(strong(catTxt[catTxt$text_type == "warning", "text"])),

      if (!is.na(inputType) & currCat != "firstPage"){
        
        if (is.element("note", catTxt$text_type)){
          
          i <- 1
          for (note in catTxt[catTxt$text_type == "note", "text"]){
            
            noteTxt <- strsplit(note, "%")[[1]]
            noteItemId <- noteTxt[1]
            noteTxt <- noteTxt[2]
            stars <- rep("*", i)
            stars <- paste0(stars, collapse = "")
            catItems[catItems$item_id == noteItemId, 'definition'] <<- paste0(catItems[catItems$item_id == noteItemId, 'definition'], stars)
            notesList[[i]] <- p(paste0(stars, noteTxt, "\n"))
            i <- i + 1
            
          }
          
        }
        
        inputObj <- renderInput()
        inputObj <- div(class=catSettings$css_class, inputObj)
        inputObj
        
      },
      
      notesList,
      
      if (catSettings$comment & currCat != "firstPage"){
        
        div(
          class = "comment",
          textAreaInput("comment", label = txt[txt$text_type == "commentLabel", "text"], value = commentValue)
        )
        
      }
      
    )
    
  })
  
}