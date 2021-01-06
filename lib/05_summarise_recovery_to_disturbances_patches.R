
library(tidyverse)
library(data.table)

# Summarise from recovery trajectories from pixel to patch ----------------

files <- list.files("recovery_trajectories", recursive = TRUE, full.names = TRUE)

k <- 0

for (file in files) {
  
  k <- k + 1
  
  print(paste0("Processing file ", k, " from ", length(files)))
  
  if (!file.exists(gsub("recovery_trajectories", "recovery_trajectories_summary", file))) {
   
    recovery_trajectories_all <- fread(file, sep = ",")
    
    recovery_trajectories_summary <- recovery_trajectories_all %>%
      mutate(since_disturbance = as.integer(year) - disturbance_year) %>%
      group_by(since_disturbance, disturbance_patch, disturbance_year, tile, country) %>%
      summarize(treecover = mean(predict_forest),
                n_pixels = n(),
                n_edge_pixels = sum(disturbance_edge),
                edge_proportion = n_edge_pixels / n_pixels,
                center_x = mean(x),
                center_y = mean(y)) %>%
      ungroup() %>%
      filter(since_disturbance > -10) %>%
      group_by(disturbance_patch, disturbance_year, tile, country) %>%
      mutate(treecover_reference = quantile(treecover[since_disturbance %in% c((-5):(-1))], 0.99)) %>%
      ungroup() %>%
      filter(since_disturbance > 0) %>%
      filter(disturbance_year > 1990) %>% # Need at least three couple of years prior the disturbance 
      mutate(treecover_rel = treecover / treecover_reference) 
    
    dir.create(dirname(gsub("recovery_trajectories", "recovery_trajectories_summary", file)), recursive = TRUE)
    
    write_csv(recovery_trajectories_summary, gsub("recovery_trajectories", "recovery_trajectories_summary", file))
     
  } else {
    
    print("Skipped file as already exists...")
    
  }
  
}


