renderCategory <- function(input, output, category, reactList, staticList){
  
  #Set current category
  reactList$category <- category
  reactList$userProgress[reactList$userProgress$type == reactList$type, "category"] <- reactList$category
  
  #Set category nr txt variable
  categoryNrTxt <- NULL
  
  #Set buttons list variable
  buttons <- NULL
  
  if (reactList$categoriesNr > 1){
    
    ### TYPE WITH MANY CATEGORIES ###
    
    #Get category items
    reactList$items <- reactList$typeItems[reactList$typeItems$category == category, ]
    
    #Get category settings
    if (is.element(category, reactList$typeSettings$category)){
      reactList$settings <- reactList$typeSettings[reactList$typeSettings$category == category, ]
    } else {
      reactList$settings <- reactList$typeSettings[reactList$typeSettings$category == "" | is.na(reactList$typeSettings$category), ]
    }
    
    if (category == reactList$firstCat){
      
      #First category
      reactList$txt <- reactList$typeTxt[reactList$typeTxt$category == category, ]
      buttons <- list(actionButton("nextBtn", label = staticList$txt[staticList$txt$text_type == "nextBtn", "text"]))
      
    } else {
      
      #Not first category
      reactList$txt <- reactList$typeTxt[reactList$typeTxt$category == category | reactList$typeTxt$category == "allInput", ]
      
      if (category == reactList$lastCat){

        #Last category 
        buttons <- list(
          actionButton("backBtn", label = staticList$txt[staticList$txt$text_type == "backBtn", "text"]),
          actionButton("confBtn", class = "btn-primary", label = staticList$txt[staticList$txt$text_type == "confBtn", "text"])
        )
        
      } else {
        
        #Not last not first category
        buttons <- list(
          actionButton("backBtn", label = staticList$txt[staticList$txt$text_type == "backBtn", "text"]),
          actionButton("nextBtn", label = staticList$txt[staticList$txt$text_type == "nextBtn", "text"])
        )
        
      }
      
    }
  
    #Get category nr and prepare string to display
    reactList$categoryNr <- match(category, reactList$categories)
    categoryNrTxt <- paste0(as.character(reactList$categoryNr), "/", as.character(reactList$categoriesNr))
    
  } else {
    
    ### TYPE WITH ONE CATEGORY ###
    
    reactList$items <- reactList$typeItems
    reactList$txt <- reactList$typeTxt
    reactList$settings <- reactList$typeSettings
    if (reactList$type != "postEnd" & reactList$type != "postEndSW") buttons <- list(actionButton("confBtn", class = "btn-primary", label = staticList$txt[staticList$txt$text_type == "confBtn", "text"]))

  }
  
  #Prepare input fields (if any)
  inputObj <- list()
  notes <- list()
  commentField <- FALSE
  if (!is.na(reactList$settings$input_type) & category != "firstPage"){
    
    #Mark current page as input
    reactList$inputPage <- TRUE
    
    #Prepare record in answers df
    record <- c(reactList$type, category, reactList$settings$input_type, NA)
    reactList$answers <- rbind(if(!tail(duplicated(rbind(reactList$answers[1:3],record[1:3])),1)) record, reactList$answers)
    
    #Get current category answer
    catAnswer <- reactList$answers[reactList$answers$type == reactList$type & reactList$answers$category == category & reactList$answers$answer_type == reactList$settings$input_type, "answer"]

    #Prepare comment field
    if (reactList$settings$comment){
      commentField <- TRUE
      record <- c(reactList$type, category, "comment", NA)
      reactList$answers <- rbind(if(!tail(duplicated(rbind(reactList$answers[1:3],record[1:3])),1)) record, reactList$answers)
      commentValue <- reactList$answers[reactList$answers$type == reactList$type & reactList$answers$category == category & reactList$answers$answer_type == "comment", "answer"]
      if (is.na(commentValue)) commentValue <- ""
    } 
    
    #Prepare notes for items
    if (is.element("note", reactList$txt$text_type)){
      i <- 1
      for (note in reactList$txt[reactList$txt$text_type == "note", "text"]){
        noteTxt <- strsplit(note, "%")[[1]]
        noteItemIds <- noteTxt[1]
        noteItemIds <- strsplit(noteItemIds, ",")[[1]]
        noteTxt <- noteTxt[2]
        stars <- rep("*", i)
        stars <- paste0(stars, collapse = "")
        for (noteItemId in noteItemIds){
          reactList$items[reactList$items$item_id == noteItemId, 'definition'] <- paste0(reactList$items[reactList$items$item_id == noteItemId, 'definition'], stars)
        }
        notes[[i]] <- HTML(paste0(stars, noteTxt, "<br>"))
        i <- i + 1
      }
      notes <- div(id="notes", notes)
    }

    #Prepare rest of input fields (inputObj)
    if (reactList$settings$input_type == "radio" | reactList$settings$input_type == "manyCheckboxGroups" | reactList$settings$input_type == "radioAlt" | reactList$settings$input_type == "checkboxAlt"){
      
      if (is.na(catAnswer)){
        selected <- c(rep("0,", nrow(reactList$items)))
      } else {
        selected <- strsplit(catAnswer, ",")[[1]]
      }
      
      if (reactList$settings$input_type != "radioAlt" & reactList$settings$input_type != "checkboxAlt"){
        choiceNames <- strsplit(reactList$txt[reactList$txt$text_type == "choiceNames", "text"], ",")[[1]]
        choiceValues <- c(1 : length(choiceNames))
      } 
      
      for (i in 1:nrow(reactList$items)){
        if (reactList$settings$input_type == "radio"){
          inputObj[[i]] <- createRadioQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], reactList$items[i, "definition"], T)
        } else if (reactList$settings$input_type == "radioAlt"){
          choiceNames <-  strsplit(reactList$items[i, "definition"], "%")[[1]]
          choiceValues <- c(1 : length(choiceNames))
          inputObj[[i]] <- list(createRadioQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i]), br())
        } else if (reactList$settings$input_type == "manyCheckboxGroups") {
          inputObj[[i]] <- createCheckboxQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], reactList$items[i, "definition"], T, noBreakInside = TRUE)
        } else if (reactList$settings$input_type == "checkboxAlt"){
          choiceNames <-  strsplit(reactList$items[i, "definition"], "%")[[1]]
          choiceValues <- c(1 : length(choiceNames))
          inputObj[[i]] <- list(br(), createCheckboxQuestion(paste0("mQ", i), choiceNames, choiceValues, selected[i], noBreakInside = FALSE))
        }
      }
      
    } else if (reactList$settings$input_type == "oneCheckboxGroup"){
      
      if (is.na(catAnswer)) catAnswer <- 0
      choiceNames <- as.character(reactList$items$definition)
      choiceValues <- c(1 : nrow(reactList$items))
      inputObj[[1]] <- createCheckboxQuestion(reactList$settings$input_type, choiceNames, choiceValues, catAnswer, noBreakInside = FALSE)
      
    } else if (reactList$settings$input_type == "sentences"){

      if (category != ""){
        reactList$sentencesNr <- as.numeric(substr(category, 10, nchar(category)))
      } else {
        reactList$sentencesNr <- as.numeric(substr(reactList$type, 10, nchar(reactList$type)))
      }

      if (is.na(catAnswer)) sentences <- c(rep("#", reactList$sentencesNr))   
      sentences <- strsplit(catAnswer, "#")[[1]]

      for (i in 1 : reactList$sentencesNr){
        inputObj[[i]] <- textInput(paste0("s", i), label = NULL, value = sentences[i])
      }

    } else if (reactList$settings$input_type == "demographic"){
      
      if (is.na(catAnswer)){
        currGender <- character(0)
        currDate <- NULL
        currFiller <- character(0)
        currFillerTxt <- ""
      } else {
        catAnswer <- strsplit(catAnswer, ",")[[1]]
        currDate <- catAnswer[1]
        currGender <- catAnswer[2]
        currFiller <- catAnswer[3]
        currFillerTxt <- catAnswer[4]
      }
      
      inputObj[[1]] <- br()
      inputObj[[2]] <- radioButtons(
        "gender",
        label = staticList$txt[staticList$txt$text_type == "genderLabel", "text"],
        selected = currGender,
        choices = strsplit(staticList$txt[staticList$txt$text_type == "genders", "text"], ",")[[1]]
      )
      inputObj[[3]] <- br()
      inputObj[[4]] <- dateInput("birthDate", staticList$txt[staticList$txt$text_type == "dateLabel", "text"], currDate, language = staticList$lang)
      inputObj[[5]] <- br()
      inputObj[[6]] <- radioButtons(
        "filler",
        label = staticList$txt[staticList$txt$text_type == "fillerLabel", "text"],
        selected = currFiller,
        choices = strsplit(staticList$txt[staticList$txt$text_type == "fillers", "text"], ",")[[1]],
      )
      inputObj[[7]] <- textInput("fillerTxt", label = NULL, value = currFillerTxt)
      
    }
    
    inputObj <- div(id="inputObj", inputObj)

  } else {
    reactList$inputPage <- FALSE
  }

  #Render sidebar
  output$sidebar <- renderUI({
    list(
      if (is.element("help", reactList$txt$text_type)) div(class="help-block", reactList$txt[reactList$txt$text_type == "help", "text"]),
      if (!is.null(categoryNrTxt)) div(id = "categoryNr", (categoryNrTxt)),
      if (!is.null(buttons)) div(id = "buttons", buttons)
    )
  })
  
  #Render main
  output$main <- renderUI({
    list(
      if (is.element("catHeader", reactList$txt$text_type)) h4(reactList$txt[reactList$txt$text_type == "catHeader", "text"]),
      if (is.element("instr", reactList$txt$text_type)) h5(reactList$txt[reactList$txt$text_type == "instr", "text"]),
      if (is.element("longText", reactList$txt$text_type)) p(reactList$txt[reactList$txt$text_type == "longText", "text"]),
      if (is.element("warning", reactList$txt$text_type)) p(class = "warning", strong(reactList$txt[reactList$txt$text_type == "warning", "text"])),
      if (length(inputObj) > 0) div(class=reactList$settings$css_class, inputObj),
      if (length(notes) > 0) notes,
      if (commentField) div(class = "comment", textAreaInput("comment", label = staticList$txt[staticList$txt$text_type == "commentLabel", "text"], value = commentValue))
    )
  })

  #Add scrolling to header when opening new category
  runjs('document.getElementById("header").scrollIntoView();')
  
  #Return updated reactiveVal
  return(reactList)
  
}