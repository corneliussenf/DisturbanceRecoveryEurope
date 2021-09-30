
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(patchwork)
library(sf)
library(raster)
library(minpack.lm)

# Load grid and countries -------------------------------------------------

grid <- read_sf("../mapping/data/gis/referencegrid/hexagon_50km.shp")

grid <- grid %>%
  dplyr::rename(gridid = id)

countries_sf <- read_sf("data/admin/countries_europe_simplyfied.shp")
outline_sf <- read_sf("data/admin/countries_europe_outline_simplyfied.shp")

cntrs <- list.files("data/recovery_trajectories_summary/")

disturbance_regime_indicators <- read_csv("data/disturbance_regime_senf_and_seidl_2021_nature_sustainability.csv")

# Create summary files ----------------------------------------------------

grid <- read_sf("../mapping/data/gis/referencegrid/hexagon_50km.shp")

grid <- grid %>%
  dplyr::rename(gridid = id)

cntrs <- list.files("data/recovery_trajectories_summary/")

### All disturbances

recovery_trajectories_summary_out <- vector("list", length(cntrs))

k <- 0

for (cntr in cntrs) {
  
  print(cntr)
  
  k <- k + 1
  
  recovery_trajectories_summary <- list.files(paste0("data/recovery_trajectories_summary/", cntr), full.names = TRUE) %>%
    map(read_csv) %>%
    bind_rows()
  
  patch_centers <- recovery_trajectories_summary %>%
    filter(since_disturbance == 1) %>%
    group_by(disturbance_patch, disturbance_year, tile) %>%
    summarize(center_x = unique(center_x),
              center_y = unique(center_y))
  
  patch_centers_sf <- st_as_sf(sp::SpatialPoints(coords = as.matrix(patch_centers[, c("center_x", "center_y")])), dim = "XY")
  
  patch_centers_sf$disturbance_patch <- patch_centers$disturbance_patch
  patch_centers_sf$disturbance_year <- patch_centers$disturbance_year
  patch_centers_sf$tile <- patch_centers$tile
  
  st_crs(patch_centers_sf) <- st_crs(grid)
  
  grid_at_patches <- st_intersection(grid, patch_centers_sf)
  
  recovery_trajectories_summary_out[[k]] <- recovery_trajectories_summary %>%
    left_join(st_drop_geometry(grid_at_patches) %>%
                dplyr::select(gridid, disturbance_patch, disturbance_year, tile)) %>%
    group_by(since_disturbance, gridid) %>%
    summarize(treecover_rel_mean = mean(treecover_rel, na.rm = TRUE),
              treecover_mean = mean(treecover, na.rm = TRUE),
              treecover_median = median(treecover, na.rm = TRUE),
              treecover_sd = sd(treecover, na.rm = TRUE),
              treecover_iqr = IQR(treecover, na.rm = TRUE),
              treecover_max = max(treecover, na.rm = TRUE),
              treecover_min = min(treecover, na.rm = TRUE),
              patches_recovered = sum(treecover_rel >= 0.99, na.rm = TRUE),
              patches_total = length(treecover_rel)) %>%
    mutate(country = cntr)
  
}

recovery_trajectories_summary_grid <- recovery_trajectories_summary_out  %>%
  bind_rows()

save(recovery_trajectories_summary_grid, file = "temp/recovery_trajectories_summary_grid.RData")
#load(file = "temp/recovery_trajectories_summary_grid.RData")

### By patch size

recovery_trajectories_summary_out <- vector("list", length(cntrs))

k <- 0

for (cntr in cntrs) {
  
  print(cntr)
  
  k <- k + 1
  
  recovery_trajectories_summary <- list.files(paste0("data/recovery_trajectories_summary/", cntr), full.names = TRUE) %>%
    map(read_csv) %>%
    bind_rows()
  
  patch_centers <- recovery_trajectories_summary %>%
    filter(since_disturbance == 1) %>%
    group_by(disturbance_patch, disturbance_year, tile) %>%
    summarize(center_x = unique(center_x),
              center_y = unique(center_y))
  
  patch_centers_sf <- st_as_sf(sp::SpatialPoints(coords = as.matrix(patch_centers[, c("center_x", "center_y")])), dim = "XY")
  
  patch_centers_sf$disturbance_patch <- patch_centers$disturbance_patch
  patch_centers_sf$disturbance_year <- patch_centers$disturbance_year
  patch_centers_sf$tile <- patch_centers$tile
  
  st_crs(patch_centers_sf) <- st_crs(grid)
  
  grid_at_patches <- st_intersection(grid, patch_centers_sf)
  
  cuts <- c(0, 11, 111, max(recovery_trajectories_summary$n_pixels))
  
  recovery_trajectories_summary_out[[k]] <- recovery_trajectories_summary %>%
    mutate(patch_size = cut(n_pixels, cuts, 
                            labels = c("<1", "1-10", ">10"),
                            include.lowest = TRUE)) %>%
    left_join(st_drop_geometry(grid_at_patches) %>%
                dplyr::select(gridid, disturbance_patch, disturbance_year, tile)) %>%
    group_by(since_disturbance, gridid, patch_size) %>%
    summarize(treecover_rel_mean = mean(treecover_rel, na.rm = TRUE),
              treecover_mean = mean(treecover, na.rm = TRUE),
              treecover_median = median(treecover, na.rm = TRUE),
              treecover_sd = sd(treecover, na.rm = TRUE),
              treecover_iqr = IQR(treecover, na.rm = TRUE),
              treecover_max = max(treecover, na.rm = TRUE),
              treecover_min = min(treecover, na.rm = TRUE),
              patches_recovered = sum(treecover_rel >= 0.99, na.rm = TRUE),
              patches_total = length(treecover_rel)) %>%
    mutate(country = cntr)
  
}

recovery_trajectories_summary_grid_patch_size <- recovery_trajectories_summary_out  %>%
  bind_rows()

save(recovery_trajectories_summary_grid_patch_size, file = "temp/recovery_trajectories_summary_grid_patch_sisze.RData")

### By severity

recovery_trajectories_summary_out <- vector("list", length(cntrs))

k <- 0

for (cntr in cntrs) {
  
  print(cntr)
  
  k <- k + 1
  
  recovery_trajectories_summary <- list.files(paste0("data/recovery_trajectories_summary/", cntr), full.names = TRUE) %>%
    map(read_csv) %>%
    bind_rows()
  
  patch_centers <- recovery_trajectories_summary %>%
    filter(since_disturbance == 1) %>%
    group_by(disturbance_patch, disturbance_year, tile) %>%
    summarize(center_x = unique(center_x),
              center_y = unique(center_y))
  
  patch_centers_sf <- st_as_sf(sp::SpatialPoints(coords = as.matrix(patch_centers[, c("center_x", "center_y")])), dim = "XY")
  
  patch_centers_sf$disturbance_patch <- patch_centers$disturbance_patch
  patch_centers_sf$disturbance_year <- patch_centers$disturbance_year
  patch_centers_sf$tile <- patch_centers$tile
  
  st_crs(patch_centers_sf) <- st_crs(grid)
  
  grid_at_patches <- st_intersection(grid, patch_centers_sf)
  
  severity_summary <- recovery_trajectories_summary %>%
    group_by(disturbance_patch, disturbance_year, tile) %>%
    summarize(severity = 1 - (min(treecover, na.rm = TRUE) / unique(treecover_reference, na.rm = TRUE))) %>%
    ungroup()
  
  cuts <- c(min(severity_summary$severity, na.rm = TRUE), 0.5, 0.9, max(severity_summary$severity, na.rm = TRUE))
  
  recovery_trajectories_summary_out[[k]] <- recovery_trajectories_summary %>%
    left_join(severity_summary) %>%
    mutate(severity = cut(severity, cuts, 
                            labels = c("<50", "50-90", ">90"),
                            include.lowest = TRUE)) %>%
    left_join(st_drop_geometry(grid_at_patches) %>%
                dplyr::select(gridid, disturbance_patch, disturbance_year, tile)) %>%
    group_by(since_disturbance, gridid, severity) %>%
    summarize(treecover_rel_mean = mean(treecover_rel, na.rm = TRUE),
              treecover_mean = mean(treecover, na.rm = TRUE),
              treecover_median = median(treecover, na.rm = TRUE),
              treecover_sd = sd(treecover, na.rm = TRUE),
              treecover_iqr = IQR(treecover, na.rm = TRUE),
              treecover_max = max(treecover, na.rm = TRUE),
              treecover_min = min(treecover, na.rm = TRUE),
              patches_recovered = sum(treecover_rel >= 0.99, na.rm = TRUE),
              patches_total = length(treecover_rel)) %>%
    mutate(country = cntr)
  
}

recovery_trajectories_summary_grid_severity <- recovery_trajectories_summary_out  %>%
  bind_rows()

save(recovery_trajectories_summary_grid_severity, file = "temp/recovery_trajectories_summary_grid_severity.RData")

# Recovery analysis -------------------------------------------------------

### All disturbances pooled

load(file = "temp/recovery_trajectories_summary_grid.RData")

recovery_rates_raw <- recovery_trajectories_summary_grid %>%
  group_by(since_disturbance, gridid) %>%
  summarise(treecover_rel_mean = mean(treecover_rel_mean),
            recovered = sum(patches_recovered) / sum(patches_total)) %>%
  ungroup()

recovery_interval_collector <- c()
recovery_rate_collector <- c()

gridids <- unique(recovery_rates_raw$gridid) %>% na.omit()

for (i in sort(gridids)) {
  
  print(paste0("Finished ", round(mean(i > sort(gridids)) * 100, 2), " %"))
  
  d <- recovery_rates_raw %>% filter(gridid == i)
  
  fit <- tryCatch(nlsLM(treecover_rel_mean ~ M + (1 - M) / (1 + exp( -k * (since_disturbance - x0))),
                        data = d,
                        start = list(M = 0, k = 0.5, x0 = 15),
                        control = list(maxiter = 150)),
                  error = function(err) return(NA))
  
  if (is.na(fit)) {
    recovery_interval_collector <- c(recovery_interval_collector, NA)
    recovery_rate_collector <- c(recovery_rate_collector, NA)
  } else {
    p <- predict(fit, newdata = data.frame(since_disturbance = 1:1000))
    p <- which(p >= 0.99)[1]
    recovery_interval_collector <- c(recovery_interval_collector,
                                     ifelse(is.na(p), 1001, p))
    recovery_rate_collector <- c(recovery_rate_collector, predict(fit, newdata = data.frame(since_disturbance = 30)))
  }
  
}

recovery_rates <- data.frame(gridid = gridids,
                             recovery_interval = recovery_interval_collector,
                             recovery_rate = recovery_rate_collector) %>%
  as_tibble() %>%
  mutate(recovery_interval = ifelse(recovery_interval < 10, 10, recovery_interval))

### By patch-size

load(file = "temp/recovery_trajectories_summary_grid_patch_size.RData")

recovery_rates_raw_patch_size <- recovery_trajectories_summary_grid_patch_size %>%
  group_by(since_disturbance, gridid, patch_size) %>%
  summarise(treecover_rel_mean = mean(treecover_rel_mean),
            recovered = sum(patches_recovered) / sum(patches_total)) %>%
  ungroup()

recovery_interval_collector <- c()

gridids <- unique(recovery_rates_raw_patch_size$gridid) %>% na.omit()
patch_sizes <- unique(recovery_rates_raw_patch_size$patch_size) %>% na.omit()

for (i in sort(gridids)) {
  
  for (p in patch_sizes) {
    
    print(paste0("Finished ", round(mean(i > sort(gridids)) * 100, 2), " %"))
    
    d <- recovery_rates_raw_patch_size %>% filter(gridid == i & patch_size == p)
    
    fit <- tryCatch(nlsLM(treecover_rel_mean ~ M + (1 - M) / (1 + exp( -k * (since_disturbance - x0))),
                          data = d, 
                          start = list(M = 0, k = 0.5, x0 = 15), 
                          control = list(maxiter = 150)),
                    error = function(err) return(NA))
    
    if (is.na(fit)) {
      recovery_interval_collector <- c(recovery_interval_collector, NA)
    } else {
      p <- predict(fit, newdata = data.frame(since_disturbance = 1:1000))
      p <- which(p > 0.95)[1]
      recovery_interval_collector <- c(recovery_interval_collector, 
                                       ifelse(is.na(p), 1001, p))
    }
    
  }
  
}

recovery_rates_patch_size <- expand_grid(gridid = gridids, patch_size = patch_sizes) %>%
  mutate(recovery_interval = recovery_interval_collector) %>%
  as_tibble()

### By severity

load(file = "temp/recovery_trajectories_summary_grid_severity.RData")

recovery_rates_raw_severity <- recovery_trajectories_summary_grid_severity %>%
  group_by(since_disturbance, gridid, severity) %>%
  summarise(treecover_rel_mean = mean(treecover_rel_mean),
            recovered = sum(patches_recovered) / sum(patches_total)) %>%
  ungroup()

recovery_interval_collector <- c()

gridids <- unique(recovery_rates_raw_severity$gridid) %>% na.omit()
severities <- unique(recovery_rates_raw_severity$severity) %>% na.omit()

for (i in sort(gridids)) {
  
  for (s in severities) {
    
    print(paste0("Finished ", round(mean(i > sort(gridids)) * 100, 2), " %"))
    
    d <- recovery_rates_raw_severity %>% filter(gridid == i & severity == s)
    
    fit <- tryCatch(nlsLM(treecover_rel_mean ~ M + (1 - M) / (1 + exp( -k * (since_disturbance - x0))),
                          data = d, 
                          start = list(M = 0, k = 0.5, x0 = 15), 
                          control = list(maxiter = 150)),
                    error = function(err) return(NA))
    
    if (is.na(fit)) {
      recovery_interval_collector <- c(recovery_interval_collector, NA)
    } else {
      p <- predict(fit, newdata = data.frame(since_disturbance = 1:1000))
      p <- which(p > 0.95)[1]
      recovery_interval_collector <- c(recovery_interval_collector, 
                                       ifelse(is.na(p), 1001, p))
    }
    
  }
  
}

recovery_rates_severity <- expand_grid(gridid = gridids, severity = severities) %>%
  mutate(recovery_interval = recovery_interval_collector) %>%
  as_tibble()

### Example recovery rates

gridid_country <- st_intersection(grid %>% filter(gridid %in% gridids), st_buffer(countries_sf, 0))

gridid_country <- gridid_country %>%
  st_drop_geometry(.) %>% 
  dplyr::select(gridid, country = COUNTRY) %>%
  group_by(gridid) %>%
  summarize(country = country[1])

#ids <- c(4762, 3330, 6052, 1585, 3855)

ids <- c(1395, 
         3330, 
         #6052, 
         #1585, 
         3855)

example_models <- recovery_rates_raw %>% 
  filter(gridid %in% ids) %>%
  split(.$gridid) %>%
  map(~ nlsLM(treecover_rel_mean ~ N + M / (1 + exp( -k * (since_disturbance - x0))),
              data = ., 
              start = list(M = 1, k = 0.5, x0 = 15, N = 0), 
              control = list(maxiter = 150)))

example_data <- example_models %>%
  map(~ data.frame(since_disturbance = 1:29,
                   treecover_rel_mean = predict(., newdata = data.frame(since_disturbance = 1:29)))) %>%
  bind_rows(.id = "gridid") %>%
  mutate(gridid = as.integer(gridid)) %>%
  left_join(gridid_country)

example_data_interval <- example_models %>%
  map(~ data.frame(since_disturbance = seq(1, 29, length.out = 1000),
                   treecover_rel_mean = predict(., newdata = data.frame(since_disturbance = seq(1, 29, length.out = 1000))))) %>%
  bind_rows(.id = "gridid") %>%
  mutate(gridid = as.integer(gridid)) %>%
  filter(treecover_rel_mean >= 0.99) %>%
  group_by(gridid) %>%
  summarize(interval = since_disturbance[1])

p <- ggplot() +
  geom_segment(data = example_data_interval,
               aes(x = 0, xend = interval,
                   y = 1, yend = 1),
               alpha = 0.75, col = "grey", linetype = "dashed") +
  geom_segment(data = example_data_interval,
               aes(x = interval, xend = interval,
                   y = 0, yend = 1,
                   col = factor(gridid)),
               alpha = 0.75, linetype = "dashed") +
  geom_point(data = example_data_interval,
             aes(x = interval,
                 y = 0,
                 col = factor(gridid)),
             size = 2, shape = 2) +
  geom_point(data = recovery_rates_raw %>%
               filter(gridid %in% ids),
             aes(x = since_disturbance, y = treecover_rel_mean, col = factor(gridid)),
             shape = 1, alpha = 0.5) +
  geom_line(data = example_data,
            aes(x = since_disturbance, y = treecover_rel_mean, col = factor(gridid))) +
  # geom_point(data = example_data %>% filter(since_disturbance == 30),
  #            aes(x = since_disturbance, y = treecover_rel_mean, col = factor(gridid)),
  #            size = 2, shape = 16) +
  # geom_text(data = example_data %>% filter(since_disturbance == 30),
  #           aes(x = since_disturbance + 0.75, y = treecover_rel_mean,
  #               col = factor(gridid), label = country),
  #           size = 2.5, hjust = 0) +
  theme_linedraw() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 30)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  labs(x = "Years since disturbance", y = "Average relative canopy cover\n(in relation to pre-disturbance)") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 11)) +
  scale_color_manual(values = c("#332288", 
                                #"#88CCEE", 
                                "#44AA99", 
                                #"#DDCC77", 
                                "#CC6677"))
  
ggsave("figures/Figure02.pdf", p, width = 3.5, height = 3.5)

### Recovery interval distribution

recovery_rates %>%
  left_join(disturbance_regime_indicators) %>%
  group_by(recovery_interval = cut(recovery_interval, c(0, 30, 100, 500, 1000, 1001))) %>%
  summarize(forest = sum(forest_ha, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(forest_p = forest / sum(forest) * 100)

recovery_rates_tmp <- recovery_rates %>%
  left_join(disturbance_regime_indicators) %>%
  mutate(w = forest_ha / sum(forest_ha, na.rm = TRUE)) %>%
  filter(!is.na(w)) %>%
  filter(!is.na(recovery_interval))

density <- density(x = log10(recovery_rates_tmp$recovery_interval), 
                   weight = recovery_rates_tmp$w,
                   na.rm = TRUE, n = 10000)

recovery_interval_density <- data.frame(recovery_interval = density$x,
                                        density = density$y)

ri_mean <- sum(recovery_rates_tmp$recovery_interval * recovery_rates_tmp$w)
ri_median <- spatstat::weighted.median(recovery_rates_tmp$recovery_interval, recovery_rates_tmp$w)

p <- ggplot(data = recovery_interval_density) +
  geom_segment(aes(x = recovery_interval, y = density, 
                   xend = recovery_interval, yend = 0, 
                   colour = recovery_interval)) + 
  geom_line(aes(x = recovery_interval, y = density)) +
  annotate("text", x = log10(400), y = 2.2, size = 3.25,
           label = paste0("Mean: ", round(ri_mean, 0), " years")) +
  # annotate("text", x = log10(400), y = 2, size = 3.25,
  #          label = paste0("Median: ", round(ri_median, 0), " years")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1, max(recovery_interval_density$recovery_interval)),
                     breaks = c(0, 1, 2, 3),
                     labels = c(10^c(0, 1, 2), ">1000")) +
  scale_color_gradientn(colours = c("#1B7837", "#5AAE61", "#ACD39E", "#D9F0D3"),
                        values = scales::rescale(c(10, 250, 1000)),
                        na.value = "red",
                        limits = c(1, max(recovery_interval_density$recovery_interval)),
                        breaks = c(1, 2, 3),
                        labels = c("10", "100", ">1000")) +
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "Years", 
       y = "Density", 
       col = NULL)

### Map recovery and disturbance intervals

# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# world <- st_transform(world, st_crs(countries_sf))
# st_crs(world) <- st_crs(countries_sf)
# world <- st_crop(world, st_bbox(countries_sf) + c(-0.05, -0.05, 0.01, 0.01) * as.double(st_bbox(countries_sf)))

selector <- unique(disturbance_regime_indicators$gridid)

recovery_rates_grid <- grid  %>%
  left_join(recovery_rates, "gridid") %>%
  left_join(disturbance_regime_indicators) %>%
  mutate(disturbance_interval = ifelse(disturbance_interval_mean > 1000, 1001, disturbance_interval_mean)) %>%
  mutate(recovery_interval = ifelse(recovery_interval < 10, 10, recovery_interval)) %>%
  filter(gridid %in% selector)

recovery_rates_centroids <- st_centroid(recovery_rates_grid)

p1 <- ggplot() +
  geom_sf(data = outline_sf, color = NA, fill = "lightgrey") +
  geom_sf(data = recovery_rates_centroids, 
          aes(fill = log10(recovery_interval), col = log10(recovery_interval), size = forest_ha)) +
  geom_sf(data = countries_sf, color = "black", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = outline_sf, color = "black", fill = NA, alpha = 0.25, size = 0.85) +
  #geom_sf(data = world, color = NA, fill = gray(0.85), size = 0.25) +
  # geom_sf(data = recovery_rates_grid, 
  #         aes(fill = log10(recovery_interval)), alpha = 0.75, col = NA) +
  # geom_sf(data = world, color = gray(0.95), fill = NA, alpha = 0.25, size = 0.25) +
  scale_fill_gradientn(colours = c("#1B7837", "#5AAE61", "#ACD39E", "#D9F0D3"),
                       values = scales::rescale(c(10, 100, 250, 500, 1000)),
                       na.value = "red",
                       limits = c(0.95, max(log10(recovery_rates_grid$recovery_interval)) + 0.05),
                       breaks = c(1, 2, 3),
                       labels = c("<10", "100", "   >1000 yr.")) +
  scale_color_gradientn(colours = c("#1B7837", "#5AAE61", "#ACD39E", "#D9F0D3"),
                        values = scales::rescale(c(10, 100, 250, 500, 1000)),
                        na.value = "grey",
                        limits = c(0.95, max(log10(recovery_rates_grid$recovery_interval)) + 0.05),
                        breaks = c(1, 2, 3),
                        labels = c("<10", "100", "   >1000 yr.")) +
  theme_void() +
  theme(legend.position = c(0, 1),
        legend.direction = "horizontal",
        legend.justification = c(0, 1),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 7),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(0.65, "cm"),
        legend.key.height = unit(0.25, "cm")) +
  coord_sf(expand = FALSE) +
  labs(fill = "Recovery interval", col = "Recovery interval") +
  scale_size_continuous(range = c(0.01, 0.8), guide = "none") +
  guides(colour = guide_colorbar(title.position = "top"),
         fill = guide_colorbar(title.position = "top"))

p2 <- ggplot() +
  geom_sf(data = outline_sf, color = NA, fill = "lightgrey") +
  geom_sf(data = recovery_rates_centroids, 
          aes(fill = log10(disturbance_interval), col = log10(disturbance_interval), size = forest_ha)) +
  geom_sf(data = countries_sf, color = "black", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = outline_sf, color = "black", fill = NA, alpha = 0.25, size = 0.85) +
  #geom_sf(data = world, color = NA, fill = gray(0.85), size = 0.25) +
  # geom_sf(data = recovery_rates_grid, 
  #         aes(fill = log10(recovery_interval)), alpha = 0.75, col = NA) +
  # geom_sf(data = world, color = gray(0.95), fill = NA, alpha = 0.25, size = 0.25) +
  scale_fill_gradientn(colours = rev(c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")),
                       values = scales::rescale(c(10, 100, 300, 500, 1000)),
                       na.value = "grey",
                       limits = c(1.65, 3.05),
                       breaks = c(1.7, 2, 2.5, 3),
                       labels = c("50", "100", "300", "   >1000 yr.")) +
  scale_color_gradientn(colours = rev(c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")),
                        values = scales::rescale(c(10, 100, 300, 500, 1000)),
                        na.value = "grey",
                        limits = c(1.65, 3.05),
                        breaks = c(1.7, 2, 2.5, 3),
                        labels = c("50", "100", "300", "   >1000 yr.")) +
  theme_void() +
  theme(legend.position = c(0, 1),
        legend.direction = "horizontal",
        legend.justification = c(0, 1),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 7),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(0.65, "cm"),
        legend.key.height = unit(0.25, "cm")) +
  coord_sf(expand = FALSE) +
  labs(fill = "Disturbance interval", col = "Disturbance interval") +
  scale_size_continuous(range = c(0.01, 0.8), guide = "none") +
  guides(colour = guide_colorbar(title.position = "top"),
         fill = guide_colorbar(title.position = "top"))

p <- p2 + p1

ggsave("figures/figure03.pdf", p, width = 7.5, height = 3.5)

ggsave("/Users/corneliussenf/Desktop/recovery_map_rupert.pdf", width = 16.5, height = 16.5)

### Recovery interval by patch-size

p1 <- recovery_rates_patch_size %>%
  left_join(disturbance_regime) %>%
  filter(!is.na(recovery_interval)) %>%
  group_by(patch_size, recovery_interval = cut(recovery_interval, 
                                               c(0, 30, 100, 1000, 1001),
                                               labels = c("0-30", "30-100", "100-1000", ">1000"))) %>%
  summarize(forest = sum(forest_ha, na.rm = TRUE)) %>%
  group_by(patch_size) %>%
  mutate(forest_p = forest / sum(forest)) %>%
  ungroup() %>%
  ggplot(., aes(x = patch_size, y = forest_p, fill = recovery_interval)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#1B7837", "#5AAE61", "#ACD39E", "#D9F0D3")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Patch-size (ha)",
       y = "Prop. forest area",
       fill = "Recovery\ninterval") +
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.grid = element_blank(),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 7),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.justification = c(0, 1),
        plot.margin = unit(c(0.4, 0.4, 0.1, 0.1), "cm")) +
  guides(color = guide_legend(title.position = "top"))

ggsave("figures/figure04a.pdf", p1, width = 3.5, height = 3.5)

### Recovery interval by severity

p2 <- recovery_rates_severity %>%
  left_join(disturbance_regime) %>%
  filter(!is.na(recovery_interval)) %>%
  group_by(severity, recovery_interval = cut(recovery_interval, 
                                             c(0, 30, 100, 1000, 1001),
                                             labels = c("0-30", "30-100", "100-1000", ">1000"))) %>%
  summarize(forest = sum(forest_ha, na.rm = TRUE)) %>%
  group_by(severity) %>%
  mutate(forest_p = forest / sum(forest)) %>%
  ungroup() %>%
  ggplot(., aes(x = severity, y = forest_p, fill = recovery_interval)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#1B7837", "#5AAE61", "#ACD39E", "#D9F0D3")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Severity (%)",
       y = "Prop. forest area",
       fill = "Recovery\ninterval") +
  theme_linedraw() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.grid = element_blank(),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.justification = c(0, 1),
        plot.margin = unit(c(0.4, 0.4, 0.1, 0.1), "cm")) +
  guides(color = guide_legend(title.position = "top"))

ggsave("figures/figure04b.pdf", p2, width = 3.5, height = 3.5)

# Combine both

p <- (p1 + theme(legend.position = "none")) + 
  (p2 + labs(y = NULL)) + 
  plot_layout(ncol = 2)

ggsave("figures/figure04.pdf", p, width = 3.5, height = 1.5)

# Resilience analysis -----------------------------------------------------

resilience <- recovery_rates %>%
  left_join(disturbance_regime_indicators, by = "gridid") %>%
  mutate(resilience = disturbance_interval_mean / recovery_interval)

resilience %>%
  group_by(resilience_classes = cut(resilience, c(0, 1, 5, 10, 10000), right = FALSE)) %>%
  summarize(forest = sum(forest_ha)) %>%
  ungroup() %>%
  mutate(forest_p = forest / sum(forest, na.rm = TRUE) * 100)

### Map

resilience_grid <- grid %>%
  left_join(resilience, by = "gridid") %>%
  filter(gridid %in% selector)

resilience_centroids <- st_centroid(resilience_grid)

p <- ggplot() +
  #geom_sf(data = world, color = NA, fill = gray(0.85), size = 0.25) +
  geom_sf(data = outline_sf, color = NA, fill = "lightgrey") +
  geom_sf(data = resilience_centroids %>%
            mutate(resilience = ifelse(resilience > 1000, 1000, resilience),
                   resilience = ifelse(resilience < 0.1, 0.1, resilience)), 
          aes(fill = log10(resilience), col = log10(resilience), size = forest_ha)) +
  #geom_sf(data = world, color = "white", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = countries_sf, color = "black", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = outline_sf, color = "black", fill = NA, alpha = 0.25, size = 0.85) +
  # scale_fill_gradientn(colours = c("#B2182B", "#D6604D", "#F7F7F7", "#92C5DE", "#2166AC"),
  #                      values = scales::rescale(c(min_stab, -1, 0, 1, max_stab)), 
  #                      breaks = c(min_stab, 0, max_stab), 
  #                      label = c("Critical", "Low", "High"),
  #                      guide = guide_colourbar(nbin = 10000)) +
  # scale_color_gradientn(colours = c("#B2182B", "#D6604D", "#F7F7F7", "#92C5DE", "#2166AC"),
  #                       values = scales::rescale(c(min_stab, -1, 0, 1, max_stab)), 
  #                       breaks = c(min_stab, 0, max_stab), 
  #                       label = c("Critical", "Low", "High"),
  #                       guide = guide_colourbar(nbin = 10000)) +
  # scale_fill_gradientn(colours = c("#B2182B", "#D6604D", "#F7F7F7", "#92C5DE", "#2166AC"),
  #                      values = scales::rescale(c(-1, -0.5, 0, 1, 3)), 
  #                      breaks = c(-1, 0, 1, 2, 3), 
  #                      label = c("<0.1\n~Critical", "1\n~Low", "10\n\u0020\u0020\u0020\u0020\u0020~High \u2192", "100", ">1000"),
  #                      guide = guide_colourbar(nbin = 10000)) +
  # scale_color_gradientn(colours = c("#B2182B", "#D6604D", "#F7F7F7", "#92C5DE", "#2166AC"),
  #                       values = scales::rescale(c(-1, -0.5, 0, 1, 3)), 
  #                       breaks = c(-1, 0, 1, 2, 3), 
  #                       label = c("<0.1\n~Critical", "1\n~Low", "10\n\u0020\u0020\u0020\u0020\u0020~High \u2192", "100", ">1000"),
  #                       guide = guide_colourbar(nbin = 10000)) +
  scale_fill_gradientn(colours = c("#B2182B", "#D6604D", "#F7F7F7", "#92C5DE", "#2166AC"),
                     values = scales::rescale(c(-1, -0.5, 0, 1, 3)), 
                     breaks = c(-1, 0, 1, 2, 3), 
                     label = c("<0.1", "1", "10", "100", ">1000"),
                     guide = guide_colourbar(nbin = 10000)) +
  scale_color_gradientn(colours = c("#B2182B", "#D6604D", "#F7F7F7", "#92C5DE", "#2166AC"),
                        values = scales::rescale(c(-1, -0.5, 0, 1, 3)), 
                        breaks = c(-1, 0, 1, 2, 3), 
                        label = c("<0.1", "1", "10", "100", ">1000"),
                        guide = guide_colourbar(nbin = 10000)) +
  theme_void() +
  theme(legend.position = c(0.02, 0.98),
        legend.direction = "horizontal",
        legend.justification = c(0, 1),
        legend.title = element_text(size = 10, hjust = 0),
        legend.text = element_text(size = 7),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(0.8, "cm"),
        legend.key.height = unit(0.25, "cm")) +
  coord_sf(expand = FALSE) +
  labs(fill = "Resilience to disturbances", col = "Resilience to disturbances") +
  guides(colour = guide_colorbar(title.position = "top"),
         fill = guide_colorbar(title.position = "top")) +
  scale_size_continuous(range = c(0.01, 3), guide = "none")

ggsave("figures/figure05.pdf", p, width = 7.5, height = 7.5)

pp1 <- ggplot() +
  geom_sf(data = outline_sf, color = NA, fill = "lightgrey") +
  geom_sf(data = resilience_centroids %>%
            mutate(resilience = ifelse(resilience > 1000, 1000, resilience),
                   resilience = ifelse(resilience < 0.1, 0.1, resilience)), 
          aes(fill = ifelse(resilience > 10, "a", NA), 
              col = ifelse(resilience > 10, "a", NA), size = forest_ha)) +
  geom_sf(data = countries_sf, color = "black", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = outline_sf, color = "black", fill = NA, alpha = 0.25, size = 0.85) +
  scale_fill_manual(values = c("#CC6677")) +
  scale_color_manual(values = c("#CC6677")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "Resilience > 10") +
  scale_size_continuous(range = c(0.01, 0.8), guide = "none")

pp2 <- ggplot() +
  geom_sf(data = outline_sf, color = NA, fill = "lightgrey") +
  geom_sf(data = resilience_centroids %>%
            mutate(resilience = ifelse(resilience > 1000, 1000, resilience),
                   resilience = ifelse(resilience < 0.1, 0.1, resilience)), 
          aes(fill = ifelse(resilience > 5 & resilience < 10, "a", NA), 
              col = ifelse(resilience > 5 & resilience < 10, "a", NA), size = forest_ha)) +
  geom_sf(data = countries_sf, color = "black", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = outline_sf, color = "black", fill = NA, alpha = 0.25, size = 0.85) +
  scale_fill_manual(values = c("#CC6677")) +
  scale_color_manual(values = c("#CC6677")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "5 < Resilience < 10") +
  scale_size_continuous(range = c(0.01, 0.8), guide = "none")

pp3 <- ggplot() +
  geom_sf(data = outline_sf, color = NA, fill = "lightgrey") +
  geom_sf(data = resilience_centroids %>%
            mutate(resilience = ifelse(resilience > 1000, 1000, resilience),
                   resilience = ifelse(resilience < 0.1, 0.1, resilience)), 
          aes(fill = ifelse(resilience > 1 & resilience < 5, "a", NA), 
              col = ifelse(resilience > 1 & resilience < 5, "a", NA), size = forest_ha)) +
  geom_sf(data = countries_sf, color = "black", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = outline_sf, color = "black", fill = NA, alpha = 0.25, size = 0.85) +
  scale_fill_manual(values = c("#CC6677")) +
  scale_color_manual(values = c("#CC6677")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "1 < Resilience < 5") +
  scale_size_continuous(range = c(0.01, 0.8), guide = "none")

pp4 <- ggplot() +
  geom_sf(data = outline_sf, color = NA, fill = "lightgrey") +
  geom_sf(data = resilience_centroids %>%
            mutate(resilience = ifelse(resilience > 1000, 1000, resilience),
                   resilience = ifelse(resilience < 0.1, 0.1, resilience)), 
          aes(fill = ifelse(resilience < 1, "a", NA), 
              col = ifelse(resilience < 1, "a", NA), size = forest_ha)) +
  geom_sf(data = countries_sf, color = "black", fill = NA, alpha = 0.25, size = 0.25) +
  geom_sf(data = outline_sf, color = "black", fill = NA, alpha = 0.25, size = 0.85) +
  scale_fill_manual(values = c("#CC6677")) +
  scale_color_manual(values = c("#CC6677")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "Resilience < 1") +
  scale_size_continuous(range = c(0.01, 0.8), guide = "none")

p <- pp1 + pp2 + pp3 + pp4 + plot_layout(ncol = 4)

ggsave("figures/SI_resilience.pdf", p, width = 7.5, height = 2.5)

