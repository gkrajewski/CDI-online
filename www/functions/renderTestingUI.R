renderTestingUI <- function(output, header, headerColor, txt, parameters, values){
  
  question = values$itemsGroup$question[values$nextItem]
  label = values$itemsGroup$item[values$nextItem]
  names = strsplit(txt[txt$text_type == "choiceNames", "text"], ",")[[1]]
  
  if (!is.null(question)) {
    question = HTML(question)
  }
  
  if (!is.null(label)) {
    label = HTML(label)
  }
  
  names <- lapply(names, function(x) if (is.null(x)) x else HTML(x))
  
  output$main <- renderUI({
    list(
      if (header %in% txt$text_type & headerColor %in% parameters$parameter) {
        h3(txt[txt$text_type == header, "text"], 
           style=paste0("color: ", parameters[parameters$parameter == headerColor, "value"], ";"))
        },
      if (header %in% txt$text_type & !headerColor %in% parameters$parameter) h3(txt[txt$text_type == header, "text"]),
      p(tags$b(question)),
      radioButtons(
        "question",
        label = label,
        selected = character(0),
        choiceNames = names,
        choiceValues = c(0,1)
      )
    )
  })
  
  output$sidebar <- renderUI({
    div(class = "help-block", txt[txt$text_type == "sidebarInstr", "text"])
  })
  
}