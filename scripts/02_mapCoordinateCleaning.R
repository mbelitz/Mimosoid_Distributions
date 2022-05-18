library(dplyr)
library(ggplot2)
library(CoordinateCleaner)
library(rnaturalearth)
library(sf)
library(stringr)
library(dbplyr)
library(RSQLite)

#' Function to clean coordinates from occurrence records
myDB <- "data/Mimosoid_Occs.db"
myConn <- dbConnect(drv = SQLite(), dbname= myDB)

map_coord_clean <- function(binomial){
  
  gbif <- tbl(myConn, "gbif") %>% 
    filter(species == binomial) %>% 
    collect() %>% 
    rename(id = gbifid) %>% 
    dplyr::mutate(specificEpithet = NA) %>% 
    dplyr::select(id, decimalLongitude, decimalLatitude, year,
                  scientificName, genus, specificEpithet, species, locality)
  
  
  idig <- tbl(myConn, "iDigBio") %>% 
    mutate(species = paste(genus, specificEpithet)) %>% 
    filter(species == binomial) %>% 
    collect() %>% 
    rename(id = coreid) %>% 
    dplyr::select(id, decimalLongitude, decimalLatitude, year,
                  scientificName, genus, specificEpithet, species, locality)
  
  sernec <- tbl(myConn, "sernec") %>% 
    mutate(species = paste(genus, specificEpithet)) %>% 
    filter(species == binomial) %>% 
    collect() %>% 
    dplyr::select(id, decimalLongitude, decimalLatitude, year,
                  scientificName, genus, specificEpithet, species, locality)
  
  ## combine datasources together
  # remove NAs n decimalLongitude & Latitude and make sure decimalLong & Lats are distinct
  cs <- rbind(gbif, idig, sernec) %>% 
    filter(decimalLongitude >= -180 & decimalLongitude <= 180) %>% 
    filter(decimalLatitude >= -90 & decimalLatitude <= 90) %>% 
    distinct(decimalLongitude, decimalLatitude, .keep_all = T) %>% 
    mutate(validName = species)
  
  
  cs2 <- clean_coordinates(cs, lon = "decimalLongitude", lat = "decimalLatitude",
                           species = "validName",
                           tests = c("capitals", "centroids", "equal",
                                     "gbif", "institutions", "outliers"),
                           verbose = T)
  
  bw <- str_replace(binomial, " ", "_")
  write.csv(x = cs2, 
            file = paste("outputs/cleanedCoordinates/", bw, ".csv", sep = ""))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  us <- ne_countries(continent = "North America", returnclass = "sf")
  
  a <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(cs, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple") + 
    coord_sf(xlim = c(min(cs$decimalLongitude) - 10, max(cs$decimalLongitude) + 10),
             ylim = c(min(cs$decimalLatitude) - 10, max(cs$decimalLatitude) + 10)) +
    ggtitle("Uncleaned") + 
    theme_bw()
  
  b <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(cs2, mapping = aes(x = decimalLongitude, y = decimalLatitude,
                                  color = .summary))  +
    scale_color_manual(values = c("dark red", "black")) +
    coord_sf(xlim = c(min(cs$decimalLongitude) - 10, max(cs$decimalLongitude) + 10),
             ylim = c(min(cs$decimalLatitude) - 10, max(cs$decimalLatitude) + 10)) +
    labs(color = "Not flagged") +
    ggtitle("Cleaned") +
    theme_bw()
  
  cp <- cowplot::plot_grid(a, b, nrow = 2, ncol = 1)
  
  ggsave(plot = cp, 
         filename = paste("outputs/cleanedCoordinatesMaps/", bw, ".png", sep = ""),
         width = 8, height = 8)
  
}

## read in species list
spp_list <- read.csv("data/namelist_Mimosoid.csv") %>% 
  mutate(validName = str_replace(Species, pattern = "_", " "))

spp <- spp_list$validName

lapply(spp, map_coord_clean)

## which species don't have maps?
run <- list.files("outputs/cleanedCoordinatesMaps/") %>% 
  word(1,1, sep = fixed(".")) %>% 
  str_replace("_", " ")

spp2 <- spp[200:1799]

for(i in seq_along(spp2)){
  
  tryCatch(map_coord_clean(binomial = spp2[i]),
           error = function(e) print(paste(spp2[i], "Error, did not run, skipping")))
  
}