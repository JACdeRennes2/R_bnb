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
  # Page stats desc
  output$host <- renderPlot({
    # Calcul du nombre total de listings par ville
    listing_counts <- airbnb_data %>% group_by(city) %>% summarise(realSum = n())
    # Calcul du nombre de Superhosts par ville
    superhost_count <- airbnb_data %>% group_by(city) %>% summarise(host_is_superhost = sum(host_is_superhost))
    superhost_count$abbr <- toupper(substr(superhost_count$city, 1, 3))
    # Fusion des deux dataframes
    listing_superhost_count <- merge(listing_counts, superhost_count)
    listing_superhost_count <- listing_superhost_count[order(listing_superhost_count$realSum, decreasing = TRUE), ]
    
    # Calcul du pourcentage de Superhosts par ville
    listing_superhost_count$perc <- round((listing_superhost_count$host_is_superhost / listing_superhost_count$realSum) * 100, 1)
    listing_superhost_count$perc <- paste0(listing_superhost_count$perc, "%")
    listing_superhost_count <- arrange(listing_superhost_count, desc(host_is_superhost))
    ggplot(listing_superhost_count, aes(x = realSum, y = abbr)) +
      geom_bar(aes(fill = "total"), stat = "identity", color = "black") +
      geom_bar(
        aes(x = host_is_superhost, y = abbr, fill = "Superhosts"),
        stat = "identity",
        color = "black"
      ) +
      scale_fill_manual(values = c("#E8E8E8", "#FC814A")) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 10000)) +
      labs(title = "Superhost to total Listings per City", x = "", y = "") +
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
  
  
  
  
})
