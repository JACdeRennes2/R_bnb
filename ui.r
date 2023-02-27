library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(leaflet)

airbnb_data <- read.csv("data/data_R_bnb.csv")
pays_liste <- unique(airbnb_data$pays)


fluidPage(
  
  titlePanel("Carte des AirBnBs en Europe"),
  
  # Sélection des pays
  sidebarLayout(
    sidebarPanel(
      selectInput("pays", "Choisir un pays:", choices = pays_liste)
    ),
    
    # Carte Leaflet interactive
    mainPanel(
      leafletOutput("carte")
    )
  )
)