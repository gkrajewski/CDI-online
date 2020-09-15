#Render one of two sidebars depending on input type
renderSidebar <- function(type, txtG){
  
  return (
    
    renderUI({
      
      #Multipage layout
      if (type == "word"){
        
        list(
          
          p(class = "help-block", txtG$helpMulti),
          actionButton("prevB", label = txtG$prevB),
          actionButton("nextB", label = txtG$nextB)
          
        )
        
      #Onepage layout
      } else {
        
        list(
          
          p(class = "help-block", txtG$helpOne),
          actionButton("submit", label = txtG$submit)
          
        )

      }
      
    })    
    
  )
  
}