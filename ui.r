library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(leaflet)

airbnb_data <- read.csv("data/data_R_bnb.csv")
pays_liste <- unique(airbnb_data$pays)
week_de <- unique(airbnb_data$period)


fluidPage(
  
  titlePanel("Carte des AirBnBs en Europe"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pays", "Choisir un pays :", choices = c("Europe", pays_liste)),
      radioButtons("time_week", "Moment de la semaine :", choices = week_de)
    ),
  
    mainPanel(
      leafletOutput("carte", height = 750, width = 900)
    )
  )
)