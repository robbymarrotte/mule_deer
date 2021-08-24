library(shiny)
library(raster)
library(sf)
library(leaflet)
library(leaflet.extras)
library(xgboost)
library(keras)
library(tensorflow)
library(leaflet.minicharts)
library(shinyjs)
library(xgboost)
library(keras)
library(tensorflow)
library(rgdal)
library(rgeos)
library(googledrive)

ui <- fluidPage(
  # Application title
  titlePanel("Hydrocarbon Impact on Mule Deer"),
  
  # Show a plot of the generated distribution
  mainPanel(tabsetPanel(tabPanel("Explorer",
                                 fluidRow(leafletOutput("intro_map",width = "150%", height = 800))),
                        tabPanel("View Impact XGB",
                                 fluidRow(leafletOutput("before_map_xgb",width = "150%", height = 400),
                                          leafletOutput("after_map_xgb",width = "150%", height = 400))),
                        tabPanel("View Impact Keras",
                                 fluidRow(leafletOutput("before_map_keras",width = "150%", height = 400),
                                          leafletOutput("after_map_keras",width = "150%", height = 400))),
                        tabPanel("Change",
                                 fluidRow(leafletOutput("diff_xgb",width = "150%", height = 400),
                                          leafletOutput("diff_keras",width = "150%", height = 400))))),
  
  absolutePanel(top = 10, right = 10, width = 300,
                style = "padding: 8px",
                actionButton("printShapes", h5(strong("Predict Impact"))),
                actionButton("reset_button", h5(strong("Reset Page")))))