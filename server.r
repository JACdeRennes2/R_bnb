library(shiny)
library(rAmCharts)


shinyServer(function(input, output) {
  
  data <- reactive({
    read.csv("data/")
  })
   
  output$boxPlot <- renderAmCharts({
    x <- data/
    boxplot(x, col = input$color, border = 'black', main = input$titre)
  })
  
})
