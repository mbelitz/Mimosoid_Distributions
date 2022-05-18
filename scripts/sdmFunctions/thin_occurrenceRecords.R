########## Thin coordinates if you want to reduce how many coordinates can be found in a certain area
library(spatstat)

thinPoints <- function(spp_df, area_sqkm, bio, method = "none"){
  ##### If method = 'none', do not thin coordinates
  if (method == "none"){
    print("     Not thinning coordinates")
    spp_df <- spp_df
    
    ##### If method = 'simple', thin coordinates down to the environmental variable raster resolution  
  } else if (method == "simple"){
    rasterResolution <- max(res(bio))
    while(min(nndist(clean_sppExtract)) < rasterResolution){
      nnD <- nndist(clean_sppExtract)
      clean_sppExtract <- clean_sppExtract[-(which(min(nnD) == nnD)[1]),]
    }
    row.names(clean_sppExtract) <- seq(nrow(clean_sppExtract))
    spp_df <- clean_sppExtract
    return(spp_df)
    
    ##### If method = 'complex' thin coordinates based off the spatial area of the hull (with larger hulls thinned down to a lower resolution)
  } else if (method == "complex"){
    # No thinning for < 100,000km2 alpha hull area
    if(area_sqkm < 100000){
      print("     Not thinning coordinates")
      spp_df <- spp_df
      
      # 25km thinning for between 100,000km2 and 250,000km2
    } else if(area_sqkm >= 100000 & area_sqkm < 250000){
      print("     Aggregating cells 5-fold for spatial thinning of coordinates")
      bio <- aggregate(bio, 5)
      biomdf <- raster::as.data.frame(bio, xy = T)
      biomdf_noNA <- na.omit(biomdf) %>%
        dplyr::rename(z = 3)
      biomdf_noNA <- mutate(biomdf_noNA, z = 1:nrow(biomdf_noNA))
      bio_2 <- rasterFromXYZ(biomdf_noNA)
      
      e <- raster::extract(bio_2, spp_df)
      
      spp_df$cell_id <- e
      spp_df_thinned <- st_as_sf(spp_df) %>%
        dplyr::group_by(cell_id) %>%
        slice(1)
      spp_df <- as_Spatial(spp_df_thinned)
      return(spp_df)
      
      # 50 km thinning for between 250,000km2 and 1,000,000km2
    } else if(area_sqkm >= 250000 & area_sqkm < 1000000){
      print("     Aggregating cells 10-fold for spatial thinning of coordinates")
      bio <- aggregate(bio, 10)
      biomdf <- raster::as.data.frame(bio, xy = T)
      biomdf_noNA <- na.omit(biomdf) %>%
        dplyr::rename(z = 3)
      biomdf_noNA <- mutate(biomdf_noNA, z = 1:nrow(biomdf_noNA))
      bio_2 <- rasterFromXYZ(biomdf_noNA)
      
      e <- raster::extract(bio_2, spp_df)
      
      spp_df$cell_id <- e
      spp_df_thinned <- st_as_sf(spp_df) %>%
        dplyr::group_by(cell_id) %>%
        slice(1)
      spp_df <- as_Spatial(spp_df_thinned)
      return(spp_df)
      
      # 100 km thinning for between 1,000,000km2 and 2,500,000km2
    } else if (area_sqkm >= 1000000 & area_sqkm < 2500000){
      print("     Aggregating cells 20-fold for spatial thinning of coordinates")
      bio <- aggregate(bio, 20)
      biomdf <- raster::as.data.frame(bio, xy = T)
      biomdf_noNA <- na.omit(biomdf) %>%
        dplyr::rename(z = 3)
      biomdf_noNA <- mutate(biomdf_noNA, z = 1:nrow(biomdf_noNA))
      bio_2 <- rasterFromXYZ(biomdf_noNA)
      
      e <- raster::extract(bio_2, spp_df)
      
      spp_df$cell_id <- e
      spp_df_thinned <- st_as_sf(spp_df) %>%
        dplyr::group_by(cell_id) %>%
        slice(1)
      spp_df <- as_Spatial(spp_df_thinned)
      return(spp_df)
      
      # 200 km thinning for > 2,500,000km2
    } else{
      print("     Aggregating cells 40-fold for spatial thinning of coordinates")
      bio <- aggregate(bio, 40)
      biomdf <- raster::as.data.frame(bio, xy = T)
      biomdf_noNA <- na.omit(biomdf) %>%
        dplyr::rename(z = 3)
      biomdf_noNA <- mutate(biomdf_noNA, z = 1:nrow(biomdf_noNA))
      bio_2 <- rasterFromXYZ(biomdf_noNA)
      
      e <- raster::extract(bio_2, spp_df)
      
      spp_df$cell_id <- e
      spp_df_thinned <- st_as_sf(spp_df) %>%
        dplyr::group_by(cell_id) %>%
        slice(1)
      spp_df <- as_Spatial(spp_df_thinned)
      return(spp_df)
    }
  }
}
