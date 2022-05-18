library(sp)
library(rangeBuilder)
library(alphahull)
library(sf)
library(tidyverse)

# Read in past spp specific workflows


#' Function to determine accessible area for SDMs. This is done using an alpha
#' hull approach, where the buffer to the alpha hull is either a user-defined 
#' percentile distance between points, a user-defined proportional increase
#' hull area, or a user-defined minimum distance in meters

define_accessibleArea <- function(species_df, 
                                  minBuff = 75000, 
                                  buff_prop = 0.8,
                                  projCRS){
  
  temp <- species_df
  
  #### Calculate buffer in meters
  ### Set projection
  coordinates(temp) <- ~ decimalLongitude + decimalLatitude
  proj4string(temp) <- CRS("+proj=longlat +datum=WGS84")
  tempTrans <- spTransform(temp, "+proj=cea +lat_ts=0 +lon_0")
  
  ### Create alpha hull and turn it into a polygon
  shape <- getDynamicAlphaHull(x = temp@coords, fraction = 1, partCount = 1, initialAlpha = 20, 
                               clipToCoast = "terrestrial", proj = "+proj=longlat +datum=WGS84")
  shapeTrans <- spTransform(shape[[1]], "+proj=cea +lat_ts=0 +lon_0=0")
  
  ### Create a buffer
  ## If the provided buff_prop value is between 0 and 1, create the buffer through the quantile method (default is 0.8)
  if (buff_prop > 0 & buff_prop < 1){
    # Calculate provided percentile distance between points
    buffDist <- quantile (x = (apply(spDists(tempTrans), 2, 
                                     FUN = function(x) sort(x)[2])), probs = buff_prop, na.rm = TRUE) 
    
    ## If it is greater than 1, create the buffer using the proportional area method
  } else if (buff_prop >= 1){
    # Calculate the width of an area proportionally larger than the original
    buffDist = (sqrt(buff_prop*gArea(shapeTrans)) - sqrt(gArea(shapeTrans)))/2
  }
  
  # Buffer distance becomes either minimum or the calculated distance made above
  buffDist <- max(buffDist, minBuff)
  
  ### Create the buffered shape
  shape2 <- raster::buffer(x = shapeTrans, width = buffDist, dissolve = T)
  
  if(proj4string(shape2) == projCRS){
    shape2
  } else{
    shape2 <- spTransform(shape2, CRSobj = projCRS)
  }
  
  return(shape2)
  
}
