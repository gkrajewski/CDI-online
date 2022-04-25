renderTestingUI <- function(output, header, headerColor, txt, parameters, values){
  
  output$main <- renderUI({
    list(
      if (header %in% txt$text_type & headerColor %in% parameters$parameter) {
        h3(txt[txt$text_type == header, "text"], 
           style=paste0("color: ", parameters[parameters$parameter == headerColor, "value"], ";"))},
      if (header %in% txt$text_type & !headerColor %in% parameters$parameter) h3(txt[txt$text_type == header, "text"]),
      p(tags$b(values$itemsGroup$question[values$nextItem])),
      radioButtons(
        "question",
        label = values$itemsGroup$item[values$nextItem],
        selected = character(0),
        choiceNames = strsplit(txt[txt$text_type == "choiceNames", "text"], ",")[[1]],
        choiceValues = c(0,1)
      )
    )
  })
  
  output$sidebar <- renderUI({
    div(class = "help-block", txt[txt$text_type == "sidebarInstr", "text"])
  })
  
}