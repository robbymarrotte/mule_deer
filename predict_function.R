predict_impact <- function(new_pad_poly){
  new_pad_poly <- st_transform(new_pad_poly,crs = proj4string(index_raster))
  new_pad_poly <- as_Spatial(new_pad_poly)
  new_pred_df <- pred_df
  
  ring_seq <- seq(100,1000,100)
  for(ring_dist in ring_seq){
    
    tmp_pad_poly <- gBuffer(new_pad_poly,width = ring_dist) # buffer it to make a medium sized pad
    
    # Identify raster pixels
    pad_rat_IDs <- cellFromPolygon(index_raster,p = tmp_pad_poly)[[1]]
    
    # Current predictor name
    current_label <- paste0("pad_drill_",ring_dist)
    
    curr_mean <- cont_means[names(cont_means) == current_label]
    curr_std <- cont_std[names(cont_std) == current_label]
    
    # Prediction data from these pixels
    new_pred_df[pad_rat_IDs,current_label] <- (((new_pred_df[pad_rat_IDs,current_label] * curr_std) + curr_mean + 1) - curr_mean)/curr_std
  }
  
  # Modify the area of the landsat data
  
  # Identify raster pixels
  new_pad_rat_IDs <- cellFromPolygon(index_raster,p = new_pad_poly)[[1]]
  PJ308 <- pads[pads$RID == "J308",]
  pad_rat_IDs <- unique(unlist(cellFromPolygon(index_raster,p = PJ308)))
  
  # Summer 4 bands, ndvi, ndvi slope
  new_pred_df$summer_b1[new_pad_rat_IDs] <- mean(new_pred_df$summer_b1[pad_rat_IDs])
  new_pred_df$summer_b2[new_pad_rat_IDs] <- mean(new_pred_df$summer_b2[pad_rat_IDs])
  new_pred_df$summer_b3[new_pad_rat_IDs] <- mean(new_pred_df$summer_b3[pad_rat_IDs])
  new_pred_df$summer_b4[new_pad_rat_IDs] <- mean(new_pred_df$summer_b4[pad_rat_IDs])
  new_pred_df$summer_ndvi[new_pad_rat_IDs] <- mean(new_pred_df$summer_ndvi[pad_rat_IDs])
  new_pred_df$summer_ndvi_slope[new_pad_rat_IDs] <- mean(new_pred_df$summer_ndvi_slope[pad_rat_IDs])
  
  # winter 4 bands, ndvi, ndvi slope
  new_pred_df$winter_b1[new_pad_rat_IDs] <- mean(new_pred_df$winter_b1[pad_rat_IDs])
  new_pred_df$winter_b2[new_pad_rat_IDs] <- mean(new_pred_df$winter_b2[pad_rat_IDs])
  new_pred_df$winter_b3[new_pad_rat_IDs] <- mean(new_pred_df$winter_b3[pad_rat_IDs])
  new_pred_df$winter_b4[new_pad_rat_IDs] <- mean(new_pred_df$winter_b4[pad_rat_IDs])
  new_pred_df$winter_ndvi[new_pad_rat_IDs] <- mean(new_pred_df$winter_ndvi[pad_rat_IDs])
  new_pred_df$winter_ndvi_slope[new_pad_rat_IDs] <- mean(new_pred_df$winter_ndvi_slope[pad_rat_IDs])
  
  # Reduce new df to size of what we actually want predicted, so create an impact area
  impact_area <- gBuffer(new_pad_poly,width = 1500)
  impact_area_IDs <- unique(unlist(cellFromPolygon(index_raster,p = impact_area)))
  new_pred_df <- new_pred_df[impact_area_IDs,]
  
  # Now predict using models, really only need to repredict the area 1km around
  pred_xgb_after <- predict(xgb_model, as.matrix(new_pred_df))
  #pred_xgb_new <- ifelse(pred_xgb_new > 0.5, 1, 0)
  pred_keras_after <- keras_model %>% predict_proba(as.matrix(new_pred_df))
  
  # Make rasters
  xgb_after <- pred_rats$xgb_before
  xgb_after[impact_area_IDs] <- pred_xgb_after
  keras_after <- pred_rats$keras_before
  keras_after[impact_area_IDs] <- pred_keras_after
  
  pred_rats_after <- stack(xgb_after,keras_after)
  names(pred_rats_after) <- c("xgb_after","keras_after")
  
  output <- stack(pred_rats,pred_rats_after)
  
  output <- crop(output,impact_area)
  
  output <- suppressWarnings(projectRaster(output,crs = CRS("+init=epsg:3857")))
  
  return(output)
}
