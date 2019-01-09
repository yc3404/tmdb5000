library(shiny)
library(DT)


shinyServer(function(input, output) {
  
  output$mt <- renderTable({
    rec(input$movie)
    
  })
  
})
