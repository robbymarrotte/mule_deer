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
source("predict_function.R")

#setwd("D:/Mule Deer/scripts/app")
# Load some of the data
drive_deauth()
public_file <- drive_get(as_id("156ildkOAgxCC-vfk0-l4mHdK465gKgoN"))
drive_download(public_file,path = "app_data.RData",overwrite = T)
load("app_data.RData")
#load("../../output/app_data_aug24.RData")

# Load the Keras model
keras_model <- load_model_tf("model/")

# Make palettes
pal <- colorNumeric(palette = c("white","blue","red"), domain = c(0,1),
                    na.color = "transparent")
pal1 <- colorNumeric(palette = c("red","white","blue"), domain = c(-1,1),
                     na.color = "transparent")