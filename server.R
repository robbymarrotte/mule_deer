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

# Serve side stuff
server <- function(input, output, session) {
  # reset button
  observeEvent(input$reset_button, {js$reset()}) 

  # Temp file name
  tmp_polys_name <- basename(tempfile())
  
  output$intro_map <- renderLeaflet({
    map <- suppressWarnings(leaflet(study_area) %>% 
      addPolygons(color = "black", weight = 3, fill = NA) %>%
      # Base groups
        addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topography") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addDrawToolbar(polylineOptions = F,
                     circleOptions = F,
                     markerOptions = F,
                     circleMarkerOptions = F,
                     targetGroup = "draw",
                     editOptions = editToolbarOptions(edit=TRUE)) %>%
      addMeasure() %>%
      addScaleBar(position = "bottomright") %>%
      addRasterImage(pred_rats_leaflet$xgb_before,project = F,
                     colors = pal,
                     opacity = 0.3,group = "XGB") %>% 
      leaflet::addLegend(pal = pal,values = seq(0,1,0.1),title = "Use") %>%
      addRasterImage(pred_rats_leaflet$keras_before,project = F,
                     colors = pal,
                     opacity = 0.3,group = "Keras") %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("Imagery","Topography","Street Map"),
        overlayGroups = c("XGB","Keras"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup("Keras") %>%
        syncWith("maps"))
  })
  
  observeEvent(input$printShapes, {
    # Get polys
    polys <- input$intro_map_draw_all_features  
    print(polys)
    #saveRDS(polys,file = "polys.RDS")
    # Turn them into sf polygons
    polys <- lapply(polys$features,FUN = function(X){
      coords <- matrix(unlist(X$geometry$coordinates),ncol = 2,byrow = T)
      poly <- st_polygon(list(coords))
      poly
    })
    polys <- st_multipolygon(polys)
    polys <- st_sfc(polys, crs = 4326)
    st_write(polys,paste0(tmp_polys_name,".shp"),delete_layer=T)
  })
  
  observeEvent(input$reset_button,{
    session$reload()
  })
 
  
  #observeEvent(input$intro_map_draw_new_feature, {
  #  print(input$intro_map_draw_new_feature)
  #})
  
  new_raster_func <- reactive({
    if(file.exists(paste0(tmp_polys_name,".shp"))){
      new_well_4326 <- read_sf(paste0(tmp_polys_name,".shp"))
    }else{
      new_well_4326 <- read_sf("test.shp")
    }
    # Send this to the prediction function
    output_stack <- predict_impact(new_pad_poly = new_well_4326)
    
    mapcenter <- suppressWarnings(st_coordinates(st_centroid(new_well_4326)))
    
    return(list("well_pad"=new_well_4326,
                "mapcenter"=mapcenter,
                "output_stack"=output_stack))
  })
  
  
  output$before_map_xgb <- renderLeaflet({
    map <- suppressWarnings(leaflet(new_raster_func()$well_pad) %>% 
      addPolygons(color = "black", weight = 3, fill = NA) %>%
      # Base groups
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
        setView(lng = new_raster_func()$mapcenter[1],
                lat = new_raster_func()$mapcenter[2],
                zoom = 15) %>%
        addScaleBar(position = "bottomright") %>%
      addRasterImage(new_raster_func()$output_stack$xgb_before,project = F,
                   colors = pal,
                   opacity = 0.3,group = "XGB") %>% 
      leaflet::addLegend(pal = pal,values = seq(0,1,0.1),title = "Use - Before") %>%
    syncWith("maps"))})
  
  output$after_map_xgb <- renderLeaflet({
    map <- suppressWarnings(leaflet(new_raster_func()$well_pad) %>% 
      addPolygons(color = "black", weight = 3, fill = NA) %>%
      # Base groups
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
        setView(lng = new_raster_func()$mapcenter[1],
                lat = new_raster_func()$mapcenter[2],
                zoom = 15) %>%
        addScaleBar(position = "bottomright") %>%
      addRasterImage(new_raster_func()$output_stack$xgb_after,project = F,
                     colors = pal,
                     opacity = 0.3,group = "XGB") %>% 
      leaflet::addLegend(pal = pal,values = seq(0,1,0.1),title = "Use - After") %>% 
      syncWith("maps"))})
  
  output$before_map_keras <- renderLeaflet({
    map <- suppressWarnings(leaflet(new_raster_func()$well_pad) %>% 
                              addPolygons(color = "black", weight = 3, fill = NA) %>%
                              # Base groups
                              addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
                              setView(lng = new_raster_func()$mapcenter[1],
                                      lat = new_raster_func()$mapcenter[2],
                                      zoom = 15) %>%
                              addScaleBar(position = "bottomright") %>%
                              addRasterImage(new_raster_func()$output_stack$keras_before,project = F,
                                             colors = pal,
                                             opacity = 0.3,group = "XGB") %>% 
                              leaflet::addLegend(pal = pal,values = seq(0,1,0.1),title = "Use - Before") %>%
                              syncWith("maps"))})
  
  output$after_map_keras <- renderLeaflet({
    map <- suppressWarnings(leaflet(new_raster_func()$well_pad) %>% 
                              addPolygons(color = "black", weight = 3, fill = NA) %>%
                              # Base groups
                              addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
                              setView(lng = new_raster_func()$mapcenter[1],
                                      lat = new_raster_func()$mapcenter[2],
                                      zoom = 15) %>%
                              addScaleBar(position = "bottomright") %>%
                              addRasterImage(new_raster_func()$output_stack$keras_after,project = F,
                                             colors = pal,
                                             opacity = 0.3,group = "XGB") %>% 
                              leaflet::addLegend(pal = pal,values = seq(0,1,0.1),title = "Use - After") %>%
                              syncWith("maps"))})
  
  output$diff_xgb <- renderLeaflet({
    map <- suppressWarnings(leaflet(new_raster_func()$well_pad) %>% 
                              addPolygons(color = "black", weight = 3, fill = NA) %>%
                              # Base groups
                              addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
                              setView(lng = new_raster_func()$mapcenter[1],
                                      lat = new_raster_func()$mapcenter[2],
                                      zoom = 15) %>%
                              addScaleBar(position = "bottomright") %>%
                              addRasterImage((new_raster_func()$output_stack$xgb_after - new_raster_func()$output_stack$xgb_before),
                                             project = F,
                                             colors = pal1,
                                             opacity = 0.3,group = "XGB") %>% 
                              leaflet::addLegend(pal = pal1,values = seq(-1,1,0.1),title = "Change") %>%
                              syncWith("maps"))})
  
  output$diff_keras <- renderLeaflet({
    map <- suppressWarnings(leaflet(new_raster_func()$well_pad) %>% 
                              addPolygons(color = "black", weight = 3, fill = NA) %>%
                              # Base groups
                              addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
                              setView(lng = new_raster_func()$mapcenter[1],
                                      lat = new_raster_func()$mapcenter[2],
                                      zoom = 15) %>%
                              addScaleBar(position = "bottomright") %>%
                              addRasterImage((new_raster_func()$output_stack$keras_after - new_raster_func()$output_stack$keras_before),
                                             project = F,
                                             colors = pal1,
                                             opacity = 0.3,group = "XGB") %>% 
                              leaflet::addLegend(pal = pal1,values = seq(-1,1,0.1),title = "Change") %>%
                              syncWith("maps"))})
  }

  
  