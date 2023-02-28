library(shiny)
library(rAmCharts)
library(tidyverse)
library(dplyr)
library(leaflet)

airbnb_data <- read.csv("data/data_R_bnb.csv", encoding = "UTF-8")
ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red", 
                                                   space = "Lab"), domain = c(0,1.2))

shinyServer(function(input, output) {
  
  airbnb_data_filtre <- reactive({
    if(input$pays == "Europe") {
      airbnb_data
    } else {
      airbnb_data |> 
        filter(pays == input$pays)
    }
  })
  
  output$carte <- renderLeaflet({
    leaflet(data = airbnb_data_filtre()) |>
      addTiles() |>
      addCircleMarkers(lng = ~ lng, 
                       lat = ~ lat, 
                       radius = ~ 10,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 25),
                       color = ~ColorPal(realSum), 
                       stroke = FALSE,  
                       fillOpacity = 0.7)
  })
  
})
