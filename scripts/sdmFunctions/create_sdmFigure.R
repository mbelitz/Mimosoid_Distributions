################ This script will create a figure to visualize the models

### Load libraries
library(ggplot2)
library(egg)

create_sdmFigure <- function(world = world, spp, r, r_pa, occ_df){
  spp = gsub("_", " ", spp)
  ## Create a plot for the model
  p1 <- ggplot() +
    geom_sf(world, mapping = aes(), fill = NA) +
    geom_tile(as.data.frame(r, xy = T) %>% na.omit() %>% rename(ClogLog = 3),
              mapping = aes(x = x, y = y, fill = ClogLog)) + 
    geom_point(occ_df, mapping = aes(x = x, y = y), color = 'black', shape = 21, alpha = 0.6) +
    coord_sf(xlim = c(min(occ_df$x) - 500, max(occ_df$x) + 500),
             ylim = c(min(occ_df$y) - 500, max(occ_df$y) + 500)) +
    scale_fill_viridis_c() +
    ggtitle(paste0(spp, " SDM", " (# coords = ", nrow(occ_df), ")")) +
    theme(legend.position = "none") +
    theme_classic()
  
  ## Create a plot for the presence-absence model
  p2 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_tile(as.data.frame(r_pa,xy = T) %>% na.omit() %>% rename(ClogLog = 3),
              mapping = aes(x = x, y = y, fill = as.character(ClogLog))) + 
    geom_point(occ_df, mapping = aes(x = x, y = y), color = 'black', shape = 21, alpha = 0.6) +
    coord_sf(xlim = c(min(occ_df$x) - 500, max(occ_df$x) + 500),
             ylim = c(min(occ_df$y) - 500, max(occ_df$y) + 500)) +
    labs(fill = "Presence") +
    scale_fill_viridis_d() +
    theme(legend.position = "none") +
    theme_classic()
  
  ## Combine these two plots into one figure
  e <- egg::ggarrange(p1, p2, nrow = 1)
  
  return(e)
}
