library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(leaflet)
library(shinythemes)
library(ggmap)
library(shinyWidgets)
library(htmltools)

airbnb_data <- read.csv("data/data_R_bnb.csv")
pays_liste <- unique(airbnb_data$pays)
week_de <- unique(airbnb_data$period)

navbarPage(title = "R bnb",
           windowTitle="R bnb",
           collapsible = TRUE,
           setBackgroundColor(
             color = c("#FF5A5F", "white"),
             gradient = "radial",
             direction = c("top", "left")
           ),
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
                   h2("Bienvenue sur R_bnb !"),
                   box(
                     imageOutput('logo')
                   ),
                   plotOutput("carte_acceuil", width = 400, height = 400)
                 )
               )
             )
           ),
           tabPanel(title = "Carte",
                    div(class="outer",
                        
                        tags$head(
                          includeCSS("style.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("carte", width="100%", height="100%"),
                        
                        absolutePanel(
                          id = "controls",
                          width = 400,
                          class = "panel panel-default",
                          top = 30, left = 10,
                          selectInput("pays", "Choisir un pays :", choices = c("Europe", pays_liste)),
                          radioButtons("time_week", "Moment de la semaine :", choices = week_de),
                          sliderInput("values_range", "Montant des locations (en €) :", min = floor(min(airbnb_data$realSum)), max = 1000, value = c(floor(min(airbnb_data$realSum)), 1000)),
                          checkboxInput("more_than_1000", "Afficher les valeurs supérieures à 1000", FALSE),
                          plotOutput("hist", height = "350px")
                        )
                    )
           ),
           tabPanel(title = "Stat desc",
                    fluidPage(
                      fullscreen = TRUE,
                      titlePanel("Statistique descriptive"), 
                        plotOutput("host"), 
                        plotOutput("pie", height= 800),
                      plotOutput("prix", height = 1000)
                    ))
)
