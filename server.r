library(shiny)
library(rAmCharts)
library(tidyverse)
library(dplyr)
library(leaflet)

airbnb_data <- read.csv("data/data_R_bnb.csv", encoding = "UTF-8")
ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red", 
                                                   space = "Lab"), domain = c(34,3000))

shinyServer(function(input, output) {
  
  airbnb_data_filtre <- reactive({
    if(input$pays == "Europe") {
      airbnb_data |> 
        filter(period == input$time_week)
    } else {
      airbnb_data |> 
        filter(pays == input$pays & period == input$time_week)
    }
  })
  
  output$carte <- renderLeaflet({
    leaflet(data = airbnb_data_filtre()) |>
      addTiles() |>
      addCircleMarkers(lng = ~ lng, 
                       lat = ~ lat, 
                       radius = ~ 10,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 32),
                       color=~ColorPal(realSum),
                       stroke = FALSE,
                       fillOpacity = 0.7, 
                       label = ~ paste(as.character(floor(realSum)), "€", "<br/><b> Note des utilisateurs: </b>", as.character(guest_satisfaction_overall), "/100")
                       )
  })
  
})
