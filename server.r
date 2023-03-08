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
