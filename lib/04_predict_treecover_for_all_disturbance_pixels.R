
# Global libraries --------------------------------------------------------

library(tidyverse)
library(raster)
library(randomForest)
library(landscapemetrics)
#devtools::source_url("https://gist.githubusercontent.com/etiennebr/9515738/raw/9f9bdcd588c3442d2d774c6513d5e8a73417df5b/as.data.table.r")

#setwd("LandTrendr/fractionmapping/")
rasterOptions(tmpdir = "temp/") 
removeTmpFiles(h = 5)

# Set country -------------------------------------------------------------

cntr <- "austria"

print(cntr)

# Load models -------------------------------------------------------------

load(file = "results/models.RData")
dat_model <- read_csv("temp/dat_model.csv")

# Make spatial predictions ------------------------------------------------

### Functions

f_bandratio <- function(x) { (x[,1] - x[,2]) / (x[,1] + x[,2]) }
f_evi <- function(x) { 2.5 * ((x[,1] - x[,2]) / (x[,1] + 6 * x[,2] - 7.5 * x[,3] + 1)) }
f_savi <- function(x) { ((x[,1] - x[,2]) / (x[,1] + x[,2] + 0.5)) * (1.5) }
f_msavi <- function(x) { (2 * x[,1] + 1 - sqrt ((2 * x[,1] + 1)^2 - 8 * (x[,1] - x[,2]))) / 2 }

### Get tiles

tiles <- list.files(paste0("landtrendr/landtrendr_", cntr), pattern = "B1") %>%
  str_split(., "_") %>%
  map(~ .[5]) %>%
  unlist() %>%
  str_split(., "\\.") %>%
  map(~ .[1]) %>%
  unlist() %>%
  str_replace("B1-", "")

if (tiles[1] == "B1") tiles <- ""

### Load disturbances

dist <- raster(paste0("../prediction/", cntr, "/disturbance_year_filtered_", cntr, ".tif"))

### Loop through tiles/years

for (tile in tiles) {
  
  if (!file.exists(paste0("recovery_trajectories/", cntr, "/recovery_trajectories_", cntr, "_", tile, ".csv"))) {
    
    print(paste0("Processing tile ", tile))
    
    # Get disturbances and identify patches
    
    tile_ref <- list.files(paste0("landtrendr/landtrendr_", cntr), pattern = tile, full.names = TRUE) %>%
      .[1] %>%
      raster(.)
    
    dist_tile <- crop(dist, tile_ref)
    dist_tile <- extend(dist_tile, tile_ref)
    extent(dist_tile) <- extent(tile_ref)
    dist_tile_patches_patches <- get_patches(dist_tile, directions = 8)
    
    if (length(dist_tile_patches_patches) != 0) { # Only process tile if it has disturbances in it!
      
      dist_tile_patches <- stack(dist_tile_patches_patches)
      dist_tile_patches <- max(dist_tile_patches, na.rm = TRUE)
      dist_tile_patches_boundaries <- get_boundaries(dist_tile, directions = 8)
      
      dist_df <- stack(dist_tile, dist_tile_patches, dist_tile_patches_boundaries) %>%
        as.data.frame(xy = TRUE) %>%
        set_names(c("x", "y", "year", "patch", "edge"))
      
      # Make predictions for each year
      
      years <- 1987:2019
      
      recovery_trajectories_tmp <- vector("list", length(years))
      
      for (band_i in 1:length(years)) {
        
        print(paste0("... year: ", years[band_i]))
        
        ### Load stack
        
        ras <- list.files(paste0("landtrendr/landtrendr_", cntr), pattern = tile, full.names = TRUE) %>%
          map(raster, band = (band_i + (min(years) - 1984))) %>% # Add years to index as stack begins in 1984
          stack(.)
        names(ras) <- c(paste0("B", c(1:5, 7)))
        NAvalue(ras) <- 0
        
        ras_df <- as.data.frame(ras)
        ras_df$disturbance_year <- dist_df$year
        ras_df$disturbance_patch <- dist_df$patch
        ras_df$disturbance_edge <- dist_df$edge
        ras_df$x <- dist_df$x
        ras_df$y <- dist_df$y
        ras_df$pixelid <- 1:nrow(ras_df)
        ras_df <- ras_df %>% mutate_all(.funs = function(x) ifelse(x < 0, NA, x))
        ras_df_nona <- na.omit(ras_df)
        ras_df_nona$year <- years[band_i]
        ras_df_nona$tile <- tile
        
        ### Calculate indices
        
        ras_df_nona$ndvi <- f_bandratio(ras_df_nona[, c(4, 3)])
        ras_df_nona$nbr <- f_bandratio(ras_df_nona[, c(4, 6)])
        ras_df_nona$nbr2 <- f_bandratio(ras_df_nona[, c(5, 6)])
        ras_df_nona$ndmi <- f_bandratio(ras_df_nona[, c(4, 5)])
        ras_df_nona$evi <- f_evi(ras_df_nona[, c(4, 3, 1)])
        ras_df_nona$evi[ras_df_nona$evi > 4] <- 4 # Catch Inf values
        ras_df_nona$evi[ras_df_nona$evi < -4] <- -4 # Catch -Inf values
        ras_df_nona$evi[is.nan(ras_df_nona$evi)] <- 0 # Catch NaN values
        ras_df_nona$savi <- f_savi(ras_df_nona[, c(4, 3)])
        ras_df_nona$msavi <- f_msavi(ras_df_nona[, c(4, 3)])
        
        names(ras_df_nona) <- paste0("mean_", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_year", "year", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_tile", "tile", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_disturbance_year", "disturbance_year", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_disturbance_patch", "disturbance_patch", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_disturbance_edge", "disturbance_edge", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_pixelid", "pixelid", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_x", "x", names(ras_df_nona))
        names(ras_df_nona) <- gsub("mean_y", "y", names(ras_df_nona))
        
        ras_df_nona$country <- factor(cntr, levels = levels(as.factor(dat_model$country)))
        
        ### Make predictions
        
        ras_df_nona$predict_forest <- predict(fit_forest, ras_df_nona)
        
        ### Save
        
        recovery_trajectories_tmp[[band_i]] <- ras_df_nona
        
      }
      
      recovery_trajectories <- recovery_trajectories_tmp %>%
        bind_rows()
      
      dir.create(paste0("recovery_trajectories/", cntr), recursive =  TRUE, showWarnings = FALSE)
      
      write_csv(recovery_trajectories, paste0("recovery_trajectories/", cntr, "/recovery_trajectories_", cntr, "_", tile, ".csv"))
      
    } else {
      
      print("Skipping tile as no disturbances found.")
      
    }
    
  } else {
    
    print(paste0("Skipping tile ", tile, " as it was lready processed."))
    
  }
  
}

print(paste0("Fisnihed ", cntr))

rm(list = ls())
