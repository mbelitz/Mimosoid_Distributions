###### Function to clip model variable layers to accessible area

clip_variableLayers <- function(rstack, accessibleArea){
  
  ### Crop file to extent of the buffer
  aa_proj <- sp::spTransform(accessibleArea, CRSobj = terra::crs(rstack,proj = T))
  r_crop <- terra::crop(rstack, aa_proj)
  r_crop <- raster::stack(r_crop)
  
  ### Mask the cropped file
  r_mask <- raster::mask(r_crop, mask = aa_proj)
  return(r_mask)
  
}
