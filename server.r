library(shiny)
library(rAmCharts)
library(tidyverse)
library(dplyr)
library(leaflet)
library(st)
library(sf)
library(ggplot2)
library(tibble)
library(gridExtra)


airbnb_data <- read.csv("data/data_R_bnb.csv", encoding = "utf-8")
europe_polygons <- st_read("data/europe_polygons.shp") 

ColorPal <-  colorQuantile(
  palette = c("yellow", "red", "brown"),
  domain = airbnb_data$realSum,
  probs = seq(0, 1, by = 0.1)
)
pal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red",
                                             space = "Lab"), domain = europe_polygons$mean)


qpal_colors <- unique(ColorPal(sort(airbnb_data$realSum)))
qpal_labs <- quantile(airbnb_data$realSum, seq(0, 1, by = 0.1))
qpal_labs <- floor(qpal_labs)
qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1]



shinyServer(function(input, output, session) {
  
  airbnb_data_histo <- reactive({
    if(input$pays == "Europe") {
      airbnb_data
    } else {
      airbnb_data |> 
        filter(pays == input$pays)
    }
  })
  
  airbnb_data_filtre <- reactive({
    if(input$pays == "Europe") {
      airbnb_data |> 
        filter(period == input$time_week, realSum > input$values_range[1], realSum < input$values_range[2])
    } else {
      airbnb_data |> 
        filter(pays == input$pays & period == input$time_week, realSum > input$values_range[1], realSum < input$values_range[2])
    }
  })
  
  lat <- reactive({
    if(input$pays == "Europe") {
      48.52
    } else {
      europe_polygons$latitudes[which(europe_polygons$pays == input$pays)]
    }
  })
  
  lng <- reactive({
    if(input$pays == "Europe") {
      3.25
    } else {
      europe_polygons$longitudes[which(europe_polygons$pays == input$pays)]
    }
  })
  
  zoom <- reactive({
    if(input$pays == "Europe") {
      4
    } else {
      12
    }
  })
  
  output$carte <- renderLeaflet({
    leaflet(data = airbnb_data_filtre()) |>
      setView(lng = lng(), lat = lat(), zoom = zoom()) |> 
      addTiles() |>
      addCircleMarkers(lng = ~ lng, 
                       lat = ~ lat, 
                       radius = ~ 10,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 32),
                       color=~ColorPal(realSum),
                       stroke = FALSE,
                       fillOpacity = 0.7, 
                       label = ~ paste(
                         as.character(floor(realSum)),
                         "€, ",
                         "Note des utilisateurs :",as.character(guest_satisfaction_overall),"/100, ",
                         "\n",
                         "Type de Chambre :",room_type,", ",
                         "Capacité :",person_capacity,"pers, ",
                         "\n",
                         "distance metro :",round(metro_dist, 2)
                       ), 
                       group = "circles")|> 
      addPolygons(data = europe_polygons,
                  fillColor = ~pal(mean),
                  fillOpacity = 0.4,
                  weight = 1,
                  color = "#BDBDC3",
                  group = "polygons") |> 
      addLegend(group = "polygons",
                position = "topright",
                pal = pal,
                values = europe_polygons$mean,
                title = HTML("Montant moyen <br>des locations <br>par pays"),
                opacity = 0.7)|> 
      addLegend(group = "circles",
                position = "topright",
                colors = qpal_colors,
                values = airbnb_data$realSum,
                title = HTML("Montant des <br>locations"),
                labels = qpal_labs,
                opacity = 0.7) 
  })
  
  observeEvent(input$more_than_1000, {
    if (input$more_than_1000) {
      max_value <- max(airbnb_data$realSum)
    } else {
      max_value <- 1000
    }
    updateSliderInput(session, "values_range", max = max_value)
  })
  
  labels <- c(floor(min(airbnb_data$realSum)), seq(1000, 3000, 500), "1000 et +")
  updateSliderInput(session, "values_range")
  
  output$hist <- renderPlot({
    ggplot(airbnb_data_histo(), aes(x = realSum, fill=period)) +
      geom_histogram(bins=20) +
      xlim(input$values_range) +
      xlab("Prix") +
      ggtitle("Répartition des airbnbs en fonction du prix") +
      labs(fill = "Période")
  })

})
