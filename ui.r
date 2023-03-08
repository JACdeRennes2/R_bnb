library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(leaflet)
library(shinythemes)

airbnb_data <- read.csv("data/data_R_bnb.csv")
pays_liste <- unique(airbnb_data$pays)
week_de <- unique(airbnb_data$period)


navbarPage(title = "R_bnb",
           theme = shinytheme("flatly"),
           tabPanel(
             title = "Accueil",
             fluidPage(
               titlePanel("Présentation du projet"),
               sidebarLayout(
                 sidebarPanel(
                   h4("À propos du projet"),
                   p("Le projet R_bnb a pour objectif d'analyser les données d'Airbnb en Europe."),
                   h4("Comment utiliser cette application ?"),
                   p("Choisissez un pays dans le menu déroulant et un moment de la semaine en cliquant sur les boutons radio."),
                   p("Vous pouvez également ajuster le montant des locations en utilisant le curseur et afficher les valeurs supérieures à 1000 en cochant la case correspondante."),
                   p("Ensuite, explorez la carte pour voir les locations Airbnb dans le pays choisi."),
                   p("Les locations sont représentées par des marqueurs de couleurs différentes selon le montant de la location."),
                   p("Cliquez sur un marqueur pour obtenir plus d'informations sur cette location.")
                 ),
                 mainPanel(
                   h2("Bienvenue sur R_bnb !")
                 )
               )
             )
           ),
           tabPanel(title = "Home page", 
                    fluidPage(
                      
                      titlePanel("Carte des AirBnBs en Europe"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pays", "Choisir un pays :", choices = c("Europe", pays_liste)),
                          radioButtons("time_week", "Moment de la semaine :", choices = week_de),
                          sliderInput("values_range", "Montant des locations (en €) :", min = floor(min(airbnb_data$realSum)), max = 1000, value = c(floor(min(airbnb_data$realSum)), 1000)),
                          checkboxInput("more_than_1000", "Afficher les valeurs supérieures à 1000", FALSE),
                          plotOutput("hist", height = "350px")
                        ),
                        
                        mainPanel(
                          leafletOutput("carte", height = 700, width = 900)
                        )
                      )
                    )
           ),
           tabPanel(title = "Stat desc",
                    "content 3")
)

