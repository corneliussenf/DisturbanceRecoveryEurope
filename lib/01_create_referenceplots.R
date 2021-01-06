
# Libraries and functions -------------------------------------------------

library(tidyverse)
library(raster)
library(sf)
library(stars)

# Sample sizes ------------------------------------------------------------

sampsize_total <- 615

add_samples <- TRUE # Set to TRUE when there are already existing samples and new sampels should be added (i.e., continuing the common id)

sampsizes <- read.table("data/reference/sample_sizes.csv", sep = ";", header = TRUE, dec = ",") %>%
  na.omit(.) %>%
  mutate(sampsize_fraction = round(weight_area * sampsize_total))

# Create gridded and tiled version of disturbance map ---------------------

forests <- list.files("LandTrendr/prediction/", pattern = glob2rx("*forest*tif"), recursive = TRUE, full.names = TRUE) %>%
  grep(glob2rx("*unmasked*"), ., invert = TRUE, value = TRUE)

for (f in forests) {
  
  cntr <- strsplit(f, "_") %>% map(., ~ strsplit(.[3], "\\.")[[1]][1]) %>% unlist(.)
  
  sampsize <- sampsizes[sampsizes$country_name_short == cntr, "sampsize_fraction"]
  
  print(paste("Sampling", sampsize, "plots for", cntr))
  
  dir.create(paste0("LandTrendr/fractionmapping/validation/", cntr), recursive = TRUE, showWarnings = FALSE)
  outpath <- paste0("LandTrendr/fractionmapping/validation/", cntr)
  
  forest <- raster(f)
  forest <- aggregate(forest, fact = 5)
  forest[sample(which(values(!is.na(forest))), sampsize)] <- 2
  forest[forest != 2] <- NA
  forest <- rasterToPolygons(forest)
  names(forest) <- "country"
  forest$country <- cntr
  
  if (add_samples) {
    
    maxid <- list.files(paste0(outpath), pattern = glob2rx("validation_samples*pixels*")) %>%
      readr::parse_number(.) %>%
      max(.)
    
    forest$valid <- (maxid + 1):((maxid + length(forest)))
      
  } else  {
  
    forest$valid <- 1:length(forest)
    
  }
  
  for (i in 1:length(forest)) {
    
    print(paste("...", i))
    
    forest_i <- forest[i, ]
    fractions_points <- raster(forest_i, res = 15)
    values(fractions_points) <- 1:ncell(fractions_points)
    fractions_points <- rasterToPoints(fractions_points, spatial = TRUE)
    
    valid <- forest@data[i, "valid"]
    
    rgdal:: writeOGR(forest_i, dsn = outpath, layer = paste0("validation_samples_", cntr, "_", valid, "_pixels"), driver = "ESRI Shapefile")
    rgdal:: writeOGR(fractions_points, dsn = outpath, layer = paste0("validation_samples_", cntr, "_", valid, "_grids"), driver = "ESRI Shapefile")
    
  }
  
  validation_pixels <- list.files(outpath, pattern = "pixels.shp$", full.names = TRUE) %>%
    map(read_sf) %>%
    do.call(what = sf:::rbind.sf, args = .)
  
  validation_grids <- list.files(outpath, pattern = "grids.shp$", full.names = TRUE) %>%
    map(read_sf) %>%
    do.call(what = sf:::rbind.sf, args = .)
  
  write_sf(st_transform(validation_pixels, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")), 
           dsn = paste0(outpath, "/validation_", cntr, "_pixels.kml"), 
           layer = paste0("validation_samples_", cntr, "_", i, "_pixels"), driver = "KML", delete_dsn = TRUE)
  
  write_sf(st_transform(validation_grids, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")), 
           dsn = paste0(outpath, "/validation_", cntr, "_grids.kml"),
           layer = paste0("validation_samples_", cntr, "_", i, "_grids"), driver="KML", delete_dsn = TRUE)
  
}

# Map reference samples ---------------------------------------------------

list.files("data/references/referenceplots", pattern = glob2rx(""))



