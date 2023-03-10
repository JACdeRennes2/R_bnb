library(shiny)
library(rAmCharts)
library(tidyverse)
library(dplyr)
library(leaflet)
library(st)
library(sf)


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
      8.25
    } else {
      europe_polygons$longitudes[which(europe_polygons$pays == input$pays)]
    }
  })
  
  zoom <- reactive({
    if(input$pays == "Europe") {
      5
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
                         "???, ",
                         "Note des utilisateurs :",as.character(guest_satisfaction_overall),"/100, ",
                         "\n",
                         "Type de Chambre :",room_type,", ",
                         "Capacit?? :",person_capacity,"pers, ",
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
      ggtitle("R??partition des airbnbs en fonction du prix") +
      labs(fill = "P??riode")
  })
  # Page stats desc
  output$host <- renderPlot({# Calcul du nombre total de listings par ville
    listing_counts <- airbnb_data |> group_by(pays) |> summarise(realSum = n())
    # Calcul du nombre de Superhosts par ville
    superhost_count <- airbnb_data |> group_by(pays) |> summarise(host_is_superhost = sum(host_is_superhost=="True"))
    superhost_count$abbr <- toupper(substr(superhost_count$pays, 1, 3))
    # Fusion des deux dataframes
    listing_superhost_count <- merge(listing_counts, superhost_count)
    listing_superhost_count <- listing_superhost_count[order(listing_superhost_count$realSum, decreasing = TRUE), ]
    
    # Calcul du pourcentage de Superhosts par ville
    listing_superhost_count$perc <- round((listing_superhost_count$host_is_superhost / listing_superhost_count$realSum) * 100, 1)
    listing_superhost_count$perc <- paste0(listing_superhost_count$perc, "%")
    listing_superhost_count <- arrange(listing_superhost_count, desc(host_is_superhost))
    ggplot(listing_superhost_count, aes(x = realSum, y = reorder(abbr, realSum))) +
      geom_bar(aes(fill = "total"), stat = "identity", color = "black") +
      geom_bar(
        aes(x = host_is_superhost, y = abbr, fill = "Superhosts"),
        stat = "identity",
        color = "black"
      ) +
      scale_fill_manual(values = c("#FF5A5F", "#00A699")) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 10000)) +
      labs(title = "Proportion d'h??b??rgeur certifi?? par ville", x = "", y = "") +
      guides(fill = guide_legend(
        nrow = 1,
        byrow = TRUE,
        title = NULL
      )) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.ticks.x = element_line(size = 0.3),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(1.2, "lines"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  
  
  
  output$carte_acceuil <- renderPlot({
    ggmap(get_stamenmap(c(left = -12, bottom = 35, right = 30, top = 63), zoom = 5,"watercolor"))+ 
      geom_point(data = europe_polygons, aes(x = longitudes, y = latitudes),
                 color = "blue", size = 5)
  })
  
})
