library(shiny)
library(rAmCharts)
library(tidyverse)
library(dplyr)
library(leaflet)

airbnb_data <- read.csv("data/data_R_bnb.csv")
ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red", 
                                                   space = "Lab"), domain = c(0,1.2))

shinyServer(function(input, output) {
  
  airbnb_data_filtre <- reactive({
    airbnb_data |> 
      filter(pays == input$pays)
  })
  
  # Afficher la carte Leaflet interactive
  output$carte <- renderLeaflet({
    leaflet(data = airbnb_data_filtre()) |>
      # Centrer la carte sur l'Europe
      addTiles() |>
      # Ajouter les marqueurs pour les points Airbnb filtrés
      addCircleMarkers(data = airbnb_data, 
                       lng = ~ lng, 
                       lat = ~ lat, 
                       radius = ~ 10,
                       clusterOptions = markerClusterOptions(),
                       color = ~ColorPal(realSum), 
                       stroke = FALSE,  
                       fillOpacity = 0.7)
  })
  
})
