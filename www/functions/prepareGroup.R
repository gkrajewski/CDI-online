prepareGroup <- function(output, input, values, txt, parameters, startThetas, subjectAge, urlString) {
  
  # STEP 1. Prepare everything for the group
  #Prepare mirtCAT design object
  values$designFile <- paste0("CATdesigns/", urlString, "-", values$subgroup, ".rds")
  
  if (file.exists(values$designFile)){

    CATdesign <- readRDS(isolate(values$designFile))
    
    loginfo(paste0(" Design file for group: ", isolate(values$subgroup), " was read in "))
    
  } else {
    
    #preapre startTheta
    if (is.na(values$subject[[paste0(values$subgroup, "Theta")]])){
      values$subject[[paste0(values$subgroup, "Theta")]] <- chooseTheta(startThetas, values$subject$gender, subjectAge, values$subgroup)
    }
    
    params <- values$itemsGroup[, c("a1", "d")]
    mirt_object <- generate.mirt_object(params, '2PL')
    
    CATdesign <- mirtCAT(mo = mirt_object, 
                         method = parameters[parameters$parameter == paste0(values$subgroup, "MirtMethod"), "value"], 
                         criteria = parameters[parameters$parameter == paste0(values$subgroup, "MirtCriteria"), "value"], 
                         start_item = parameters[parameters$parameter == paste0(values$subgroup, "MirtCriteria"), "value"],
                         design_elements = TRUE, 
                         design = list(thetas.start = values$subject[[paste0(values$subgroup, "Theta")]]))
    
    loginfo(paste0(" Design file for group: ", isolate(values$subgroup), " was created "))
  }
  
  values$nextItem <- findNextItem(CATdesign)
  
  #Set maximum number of items in test (stop criterion)
  if (paste0(isolate(values$subgroup), "maxItemNr") %in% parameters$parameter) {
    values$maxItemNr <- as.numeric(parameters[parameters$parameter == paste0(isolate(values$subgroup), "maxItemNr"), "value"])
  } else {
    values$maxItemNr <- nrow(values$itemsGroup)
  }
  
  if (isolate(values$maxItemNr)>nrow(values$itemsGroup)) {
    values$maxItemNr <- nrow(values$itemsGroup)
  }
  
  #Set minimum number of items in test 
  if (paste0(isolate(values$subgroup), "minItemNr") %in% parameters$parameter) {
    values$minItemNr <- as.numeric(parameters[parameters$parameter == paste0(isolate(values$subgroup), "minItemNr"), "value"])
  } else {
    values$minItemNr <- 0
  }
  
  if (isolate(values$minItemNr)>isolate(values$maxItemNr) | isolate(values$minItemNr)>nrow(values$itemsGroup)) {
    stop("Minimum item number greater than maximum or available number of items")
  }
  
  #Set se_theta value 
  if (paste0(isolate(values$subgroup), "MirtSeTheta") %in% parameters$parameter) {
    values$seTheta <- as.numeric(parameters[parameters$parameter == paste0(isolate(values$subgroup), "MirtSeTheta"), "value"])
  } else {
    values$seTheta <- 0
  }
  
  loginfo(paste0(" Min number: ", isolate(values$minItemNr), " max number: ", isolate(values$maxItemNr), 
                 " se theta: ", isolate(values$seTheta)))
  
  #STEP 2. Start test and display
  
  #save the start time
  if (is.na(values$subject[[paste0(values$subgroup, "Start")]])) {
    values$subject[[paste0(values$subgroup, "Start")]] <- as.character(Sys.time())
  }
  
  #prepare headers for given group
  header <- paste0(isolate(values$subgroup), "Header")
  headerColor <- paste0(isolate(values$subgroup), "HeaderColor")
  
  #Render testing UI, display instruction first if available
  instrID <- paste0(isolate(values$subgroup), "Instr") 
  longID <- paste0(isolate(values$subgroup), "LongText") 
  warningID <- paste0(isolate(values$subgroup), "Warning") 
  
  if (is.element(instrID, txt$text_type) | 
      is.element(longID, txt$text_type) | is.element(warningID, txt$text_type)) {
    
    output$main <- renderUI(list(
    
      if (header %in% txt$text_type & headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"], style=paste0("color: ", txt[txt$text_type == headerColor, "text"], ";")),
      if (header %in% txt$text_type & !headerColor %in% txt$text_type) h3(txt[txt$text_type == header, "text"]),
      if (is.element(instrID, txt$text_type)) h5(txt[txt$text_type == instrID, "text"]),
      if (is.element(longID, txt$text_type)) p(txt[txt$text_type == longID, "text"]),
      if (is.element(warningID, txt$text_type)) p(class = "warning", strong(txt[txt$text_type == warningID, "text"]))

    ))
    
    output$sidebar <- renderUI({
      actionButton(instrID, label = txt[txt$text_type == "continueBtn", "text"], class = "btn-primary")
    })
    
    observeEvent(input[[instrID]], {
      
      renderTestingUI(output, header, headerColor, txt, parameters, values)
      
    }, once = TRUE)
    
  } else {
    
    renderTestingUI(output, header, headerColor, txt, parameters, values)
    
  }
  
  return(CATdesign)
}