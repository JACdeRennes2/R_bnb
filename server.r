library(shiny)
library(rAmCharts)
library(tidyverse)
library(dplyr)
library(leaflet)
library(st)
library(sf)


airbnb_data <- read.csv("data/data_R_bnb.csv", encoding = "utf-8")
europe_polygons <- st_read("data/NUTS_RG_20M_2021_3035.shp") 

mean_prices <- airbnb_data |> 
  group_by(pays, period) |> 
  filter(period=="weekends") |> 
  summarise(mean(realSum))

europe_polygons <- st_transform(europe_polygons, crs = 4326)
europe_polygons <- europe_polygons[c(11, 5, 24, 1, 22, 3, 62, 70, 28, 43), c(2,4,10)]
europe_polygons$NAME_LATN <- c("Allemagne", "Autriche", "Espagne", "France", "Grece", "Hongrie", "Italie", "Pays-Bas", "Portugal", "Royaume-Uni")
names(europe_polygons) <- c("LEVL_CODE","pays", "geometry")
europe_polygons$LEVL_CODE <- mean_prices$`mean(realSum)`
europe_polygons$LEVL_CODE <- round(europe_polygons$LEVL_CODE)
names(europe_polygons) <- c("mean", "pays", "geometry")


ColorPal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red", 
                                                  space = "Lab"), domain = c(34,3000))
pal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red",
                                             space = "Lab"), domain = europe_polygons$mean)


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
      setView(lng = 8.25, lat = 48.52, zoom = 5) |> 
      addTiles() |>
      addCircleMarkers(lng = ~ lng, 
                       lat = ~ lat, 
                       radius = ~ 10,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 32),
                       color=~ColorPal(realSum),
                       stroke = FALSE,
                       fillOpacity = 0.7, 
                       label = ~ paste(as.character(floor(realSum)), "€", "<br/><b> Note des utilisateurs: </b>", as.character(guest_satisfaction_overall), "/100")
                       )|> 
      addPolygons(data = europe_polygons,
                  fillColor = ~pal(mean),
                  fillOpacity = 0.7,
                  weight = 1,
                  color = "#BDBDC3")
  })
  
})
