
library(raster)
library(tidyverse)
library(sf)

cntr <- "sweden"
bands <- paste0("B", 4:5)
#bands <- paste0("B", c(1:5, 7))

print(cntr)

plots <- list.files(paste0("validation/", cntr), pattern = "grids.shp$", full.names = TRUE) 

plots <- plots %>%
  map(read_sf) %>%
  map2(.y = plots, ~ mutate(., gridid = as.integer(unlist(map(strsplit(basename(.y), "_"), ~.[4]))))) %>%
  do.call(what = sf:::rbind.sf, args = .) %>%
  dplyr::rename(pixelid = layer)

#band <- "B1"

for (band in bands) {
  
  print(band)
  
  fvt_list <- list.files(paste0("landtrendr/landtrendr_", cntr), ".tif", full.names = TRUE)
  
  fvt_list <- grep(band, fvt_list, value = TRUE)
  
  plots_extract <- fvt_list %>%
    map(stack) %>%
    map(~ raster::extract(., plots))
  
  plots_extract <- plots_extract %>%
    map(as.data.frame) %>%
    map(~ set_names(., paste(band, 1984:2019, sep = "."))) %>%
    map(~ mutate(., pixelid = plots$pixelid)) %>%
    map(~ mutate(., gridid = plots$gridid)) %>%
    bind_rows() %>%
    na.omit() %>%
    gather(key = key, value = value, -pixelid, -gridid) %>%
    separate("key", c("band", "year"), "\\.") %>%
    mutate(year = as.integer(year)) %>%
    mutate(country = cntr)
  
  write_csv(plots_extract, paste0("extraction/extraction_", cntr, "_", band, ".csv"))
  
}
