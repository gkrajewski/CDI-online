#Render one of two sidebars depending on input type
renderSidebar <- function(type, texts){
  
  return (
    
    renderUI({
      
      list(
        
        p(class = "help-block", texts[texts$text_type == "help", "text"]),
        
        #Multipage layout
        if (type == "word"){
          
          list(
            
            actionButton("prevB", label = texts[texts$text_type == "prevB", "text"]),
            actionButton("nextB", label = texts[texts$text_type == "nextB", "text"])
            
          )
          
        #Onepage layout
        } else {
          
          actionButton("submit", label = texts[texts$text_type == "submit", "text"])
          
        }
        
      )
      
    })    
    
  )
  
}