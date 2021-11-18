prepareGroup <- function(output, input, values, txt, startThetas, subjectAge, urlString) {
  
  # STEP 1. Prepare everything for the group
  
  #Prepare mirtCAT design object
  values$designFile <- designFile <- paste0("CATdesigns/", urlString, "-", values$subgroup, ".rds")
  
  if (file.exists(values$designFile)){

    CATdesign <- readRDS(isolate(values$designFile))
    
  } else {
    
    #preapre startTheta
    if (is.na(values$subject[[paste0(values$subgroup, "Theta")]])){
      values$subject[[paste0(values$subgroup, "Theta")]] <- chooseTheta(startThetas, values$subject$gender, subjectAge, values$subgroup)
    }
    
    params <- values$itemsGroup[, c("a1", "d")]
    mirt_object <- generate.mirt_object(params, '2PL')
    CATdesign <- mirtCAT(mo = mirt_object, 
                         method = txt[txt$text_type == paste0(values$subgroup, "MirtMethod"), "text"], 
                         criteria = txt[txt$text_type == paste0(values$subgroup, "MirtCriteria"), "text"], 
                         start_item = txt[txt$text_type == paste0(values$subgroup, "MirtCriteria"), "text"],
                         design_elements = TRUE, 
                         design = list(thetas.start = values$subject[[paste0(values$subgroup, "Theta")]]))
    
  }
  
  values$nextItem <- findNextItem(CATdesign)
  
  #Set maximum number of items in test (stop criterion)
  if (paste0(isolate(values$subgroup), "maxItemNr") %in% txt$text_type) {
    values$maxItemNr <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "maxItemNr"), "text"])
  } else {
    values$maxItemNr <- nrow(values$itemsGroup)
  }
  
  if (isolate(values$maxItemNr)>nrow(values$itemsGroup)) {
    values$maxItemNr <- nrow(values$itemsGroup)
  }
  
  #Set minimum number of items in test 
  if (paste0(isolate(values$subgroup), "minItemNr") %in% txt$text_type) {
    values$minItemNr <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "minItemNr"), "text"])
  } else {
    values$minItemNr <- 0
  }
  
  if (isolate(values$minItemNr)>isolate(values$maxItemNr) | isolate(values$minItemNr)>nrow(values$itemsGroup)) {
    stop("Minimum item number greater than maximum or available number of items")
  }
  
  #Set se_theta value 
  if (paste0(isolate(values$subgroup), "MirtSeTheta") %in% txt$text_type) {
    values$seTheta <- as.numeric(txt[txt$text_type == paste0(isolate(values$subgroup), "MirtSeTheta"), "text"])
  } else {
    values$seTheta <- 0
  }
  
  #STEP 2. Start test and display
  
  #save the start time
  if (is.na(values$subject[[paste0(values$subgroup, "Start")]])) {
    values$subject[[paste0(values$subgroup, "Start")]] <- as.character(Sys.time())
  }
  
  #prepare headers for given group
  header <- paste0(isolate(values$subgroup), "Header")
  headerColor <- paste0(isolate(values$subgroup), "HeaderColor")
  
  #First display instruction if available
  instrID = paste0(isolate(values$subgroup), "Instr")  
  if (instrID %in% txt$text_type) {

    output$main <- renderUI({h5(txt[txt$text_type == instrID, "text"])})
    
    output$sidebar <- renderUI({
      actionButton(instrID, label = txt[txt$text_type == "continueBtn", "text"], class = "btn-primary")
    })
    
    observeEvent(input[[instrID]], {
      
      renderTestingUI(output, header, headerColor, txt, values)
      
    })
    
  } else {
    
    renderTestingUI(output, header, headerColor, txt, values)
    
  }
  
  return(CATdesign)
}