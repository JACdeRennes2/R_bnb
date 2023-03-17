library(shiny)
library(rAmCharts)
library(tidyverse)
library(dplyr)
library(leaflet)
library(st)
library(sf)
library(tibble)
library(ggplot2)
library(gridExtra)


airbnb_data <- read.csv("data/data_R_bnb.csv", encoding = "utf-8", stringsAsFactors = TRUE)
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

europe_polygons$longitudes <- c(13.4, 16.37, 2.17, 2.33, 23.73, 19.04, 12.5, 4.90, -9.14, -0.13)
europe_polygons$latitudes <- c(52.52, 48.21, 41.39, 48.87, 37.98, 47.5, 41.9, 52.37, 38.72, 51.51)


ColorPal <-  colorQuantile(
  palette = c("#00ff00","yellow", "red"),
  domain = airbnb_data$realSum,
  probs = seq(0, 1, by = 0.1)
)
pal <- colorNumeric(scales::seq_gradient_pal(low = "#FFCEE6", high = "#360167",
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
  
  output$logo <- renderImage({
    list(src = "data/logo_rbnb_s.png",
         alt = "This is alternate text"
    )
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
      scale_fill_manual(values = c("#360167", "#FFCEE6")) +
      xlim(input$values_range) +
      xlab("Prix") +
      ggtitle("Répartition des airbnbs en fonction du prix") +
      labs(fill = "Période")
  })
  # Page stats desc
  output$host <- renderPlot({
    # Calcul du nombre total de listings par ville
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
      labs(title = "Proportion d'hébérgeur certifié par ville", x = "", y = "") +
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
        plot.title = element_text(hjust = 0.5,size = 15, face = 'bold')
      )+theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
            plot.title.position = "plot")
  })
  # Stat desc 
  output$pie <- renderPlot({
    #Pie chart de home type 
    df1 = data.frame(summary(airbnb_data$room_type))
    df1 <- tibble::add_column(df1, row.names = row.names(df1), .before = 1)
    colnames(df1) <- c("Type", "Count")
    
    
    p1 <- ggplot(df1, aes(x = "", y = Count, fill = Type)) + 
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y")  +
      geom_text(aes(label = paste0(round(Count/sum(Count)*100), "%")),
                position = position_stack(vjust = 0.5), size = 5)+
      ggtitle("Répartition des différents type de bien")+
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
                plot.title.position = "plot")
    rm(df1)
    
    # Pie chart des person capacity
    df2 = data.frame(table(airbnb_data$person_capacity))
    colnames(df2) <- c("Type", "Count")
    
    
    p2 <- ggplot(df2, aes(x = "", y = Count, fill = Type)) + 
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y")  +
      geom_text(aes(label = paste0(round(Count/sum(Count)*100), "%")),
                position = position_stack(vjust = 0.5), size = 5)+
      ggtitle("Person capcity Graph")+
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
            plot.title.position = "plot")
    rm(df2)
    
    # Pie Chart de biz
    
    df3 = data.frame(table(airbnb_data$biz))
    colnames(df3) <- c("Type", "Count")
    df3$Type <- ifelse(df3$Type == 0, "Non Pro", "Uniquement Pro")
    
    p3 <- ggplot(df3, aes(x = "", y = Count, fill = Type)) + 
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y")  +
      geom_text(aes(label = paste0(round(Count/sum(Count)*100), "%")),
                position = position_stack(vjust = 0.5), size = 5)+
      ggtitle("Répartiton des airbnb réservés aux professionels")+
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
            plot.title.position = "plot")
    rm(df3)
    
    # hist de guest_satisfaction_overall
    df4 <- data.frame(x = airbnb_data$guest_satisfaction_overall)
    df4$x[which(df4$x<50)] <- 50  
    p4 <- ggplot(df4, aes(x = x)) +
      geom_histogram((aes(y = after_stat(density))),binwidth = 5, boundary = 0, fill = "lightblue", color = "black") +
      labs(x = "Notes", y = "Nombre d'observations", title = "Histogramme des notes des apat")+
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
            plot.title.position = "plot")
    
    # Pie chart de Cleanliness_rating
    
    df5 = data.frame(table(airbnb_data$cleanliness_rating))
    colnames(df5) <- c("Type", "Count")
    # Remplacer les valeurs de Type inférieures à 7 par "Autres"
    df5$Type <- ifelse(df5$Type %in% levels(df5$Type)[1:5], "Autres", as.character(df5$Type))
    
    # Regrouper les comptages par Type
    df5 <- aggregate(df5$Count, by = list(Type = df5$Type), sum)
    
    p5 <- ggplot(df5, aes(x = "", y = x, fill = Type)) + 
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y")  +
      geom_text(aes(label = paste0(round(x/sum(x)*100), "%")),
                position = position_stack(vjust = 0.5), size = 5)+
      ggtitle("Notes de propreté des airbnb")+
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
            plot.title.position = "plot")
    rm(df5)

    # métro distance 
    df6 <- data.frame(x = airbnb_data$metro_dist)
    p6 <- ggplot(data = df6, aes(x = x,y = ..density..)) +
      geom_histogram(bins = 15, fill = "blue", alpha = 0.5) +
      ggtitle("Histogrme de la distance au métro le plus proche") +
      xlab("Distance en km") +
      ylab("Fréquence")+
      theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), 
            plot.title.position = "plot")
    
    rm(df6)
    
    grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
    rm(p1,p2,p3,p4,p5,p6)
  })
  # Graphique du prix moyen semaine
  output$prix <- renderPlot({
    ggplot(airbnb_data, aes(x = period, y = log(realSum), fill = period)) +
      geom_boxplot() +
      facet_wrap(~ pays, ncol = 2) +
      scale_fill_manual(values = c("weekdays" = "lightblue", "weekends" = "pink")) +
      labs(title = "Prix semaine et weekend par pays",
           x = "Période",
           y = "log(Prix (en euros))") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
            legend.position = "none") +
      coord_flip()
    
  })
  
  
  
  
  output$carte_acceuil <- renderPlot({
    ggmap(get_stamenmap(c(left = -12, bottom = 35, right = 30, top = 63), zoom = 5,"watercolor"))+ 
      geom_point(data = europe_polygons, aes(x = longitudes, y = latitudes),
                 color = "blue", size = 5)
  })
  

})
