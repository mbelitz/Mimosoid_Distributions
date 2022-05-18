#######################################################################################
########### This script creates and executes the pipeline that passes various functions
########### to produce species distribution models for the Mimosoid project.

############################ Part 1: Set up the pipeline #############################

#### Step 1: Load in the necessary libraries

library(rnaturalearth)
library(rgeos)
library(dismo)
library(ENMeval)
library(dplyr)
library(stringr)

#### Step 2: Load in the pipeline scripts
### 2A. Set the working directory
#setwd("/blue/soltis/tkinser/CPP/SDMs/") #uncomment this line of code if you need to set the working directory

### 2B. Source the scripts
source("scripts/sdmFunctions/define_accessibleArea.R")
source("scripts/sdmFunctions/clip_modelLayers.R")
source("scripts/sdmFunctions/thin_occurrenceRecords.R")
source("scripts/sdmFunctions/select_modelVariables.R")
source("scripts/sdmFunctions/save_sdmOutputsTSS.R")
source("scripts/sdmFunctions/create_sdmFigure.R")


#### Step 3: Prepare the spatial data

### 3A. Load in model variables
varlist <- list.files("allVars", full.names = TRUE)
mod_vars <- rast(varlist)

### 3B. Aggregate model variables to desired resolution for the model
print("Aggregating environmental variables from 1km x 1km resolution to 5km x 5km for modeling")
mod_vars <- terra::aggregate(mod_vars, 25, fun = "mean") #25 for testing

### 3C. load occurrence records
occs <- data.table::fread("outputs/cleanedOccs.csv")

### 3D. Read in basemap for visualizing
world <- ne_countries(scale = "medium", returnclass = "sf")

### 3E. Choose projection and project data if necessary
study_proj <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs" #mollweide projection
world <- st_transform(world, crs = study_proj)
mod_vars <- terra::project(mod_vars, study_proj)
occs_sf <- st_as_sf(occs, 
                    coords = c("decimalLongitude", "decimalLatitude"),
                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
occs_sf <- st_transform(occs_sf, crs = study_proj)
occs <- occs %>% 
  mutate(x = st_coordinates(occs_sf)[,1], y = st_coordinates(occs_sf)[,2])

############################ Part 2: Create the pipeline #############################

run_spp_pipeline <- function(binomial, iter){
  
  print(paste0("Producing SDM for ", binomial))
  
  cleanedOccs <- occs %>% 
    filter(validName == binomial)
  
  ### 1B. Define the accessible area for modeling
  aa_shp <- define_accessibleArea(species_df = cleanedOccs, minBuff = 75000,
                                  buff_prop = 0.80, projCRS = study_proj)
  
  ### 1C. Clip environmental variable layers to the defined accessible area
  mod_vars <- clip_variableLayers(rstack = mod_vars, accessibleArea = aa_shp)
  
  # The first time to perform the above function currently produces an error, 
  # so if this is the beginning of the loop, run function a second time 
  if (iter == 1){
    mod_vars <- clip_variableLayers(rstack = mod_vars, accessibleArea = aa_shp)
  }
  
  
  #### Step 2: Spatial thinning of coordinates
  
  ### 2A. Prepare the coordinates for rarefaction
  spp_df <- cleanedOccs
  coordinates(spp_df) <- ~ x + y
  
  ### 2B. Perform the rarefaction
  area_sqkm <- raster::area(aa_shp)/1000000
  spp_df <- thinPoints(spp_df = spp_df, area_sqkm = area_sqkm, bio = mod_vars[[2]], method = "none")
  
  #### Step 3: Test and fine-tune the model
  
  ### 3A. Select top performing variables to reduce colinearity
  ## First run a test model
  print("     Running Maxent test model")
  max_model <- maxent(x = mod_vars, p = coordinates(spp_df), progress = "text") 
  ## Using the test model, iteratively test the colinearity of variables, removing highly colinear ones one at a time 
  print("Selecting top SDM variables")
  predictors <- select_sdmVariables(pred_vars = mod_vars, maxent_mod = max_model, maxVIF = 5)
  
  ### 3B. Evaluate various tuning parameters of the model for fine tuning to create the best performing model with best tuned parameters
  print("Evaluating tuning variables in model")
  eval1 <- ENMeval::ENMevaluate(occ = coordinates(spp_df), env = predictors,
                                method = "block", RMvalues = c(0.5, 1, 2, 3, 4),
                                fc= c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                                parallel = TRUE, numCores = 5, algorithm = 'maxent.jar')
  
  #### Step 4: Create the output for the best performing model
  
  ### 4A. Prepare the output path and coordinates
  bw <- stringr::str_replace(binomial, " ", "_") # make a binomial string without a space for better saving
  
  resultDir = paste("sdmOutputs/", bw, "/", sep = "")
  if (!dir.exists(resultDir)){
    dir.create(resultDir)
  }
  # Return coordinates to a data frame
  spp_df2 <- as.data.frame(spp_df) 
  
  ### 4B. Output the model
  ## Output the model evaluation
  save(eval1, file = paste(resultDir, bw, "_ENMeval", ".RData", sep = ""))
  ## Output the best performing model, including both the actual model and the presence-absence model
  print("Saving best model")
  save_SDM_results(ENMeval_output = eval1, AUCmin = 0.7, resultDir = resultDir,
                   spp = bw, occ_df = spp_df2)
  
  ### 4C. Visualize the model
  ## Load in the rasters
  r <- raster(paste0(resultDir, bw,"_SDM.tif"))
  r_pa <- raster(paste0(resultDir, bw, "_SDM_PA.tif"))
  
  fig <- create_sdmFigure(spp = bw, r = r, r_pa = r_pa, 
                          occ_df = spp_df2, 
                          world = world)
  
  ggsave(filename = paste0("sdmMaps/",bw,"_map.png"), plot = fig,
         width = 8, height = 5)
  
  print(paste0("SDM for ", gsub("_", " ", bw), " complete"))
}


############################ Part 3: Execute the pipeline #############################

#### Step 1: Set up species list

spp_list <- read.csv("data/namelist_Mimosoid.csv")
spp_list <- stringr::str_replace(spp_list$Species, "_", " ")

# looping through pipeline
#test on 1 species
#run_spp_pipeline(binomial = spp_list[1], it = 1)

# run rest of species
#### Step 2: Pass pipeline through the species list
for(i in seq_along(spp_list)){
  tryCatch(run_spp_pipeline(binomial = spp_list[i], it = i),
           error = function(e) print(paste(spp_list[i], "Error in Code Skipping for now!")))
}
