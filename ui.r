library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)


# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Premiers pas avec shiny",
             
       tabPanel("Data", 
                navlistPanel(
                  "Titre de la structure de séléction",
                  tabPanel("table", dataTableOutput("table")),
                  tabPanel("summary", verbatimTextOutput("summary"))
                )
       ), 
       
       tabPanel("Visualisation",
           fluidRow(
             column(
               width = 3,
               sliderInput("bins",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30),
               colourInput(inputId = "color", label = "Couleur :", value = "purple"),
               textInput(inputId = "titre", label = "Titre :"),
               radioButtons(inputId = "Pays", label = "Pays : ", choices = colnames(faithful)),
             ),
             column(
               width = 9,
               tabsetPanel(id="viz",
                  tabPanel("histogramme", amChartsOutput("distPlot"))
                  )
           ))
              
       )
  )
)
