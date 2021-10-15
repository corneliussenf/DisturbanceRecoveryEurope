
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(patchwork)
library(sf)
library(raster)
library(minpack.lm)

# Create summary files ----------------------------------------------------

cntrs <- list.files("data/recovery_trajectories_summary/")

### By country

recovery_trajectories_summary_out <- vector("list", length(cntrs))

k <- 0

for (cntr in cntrs) {
  
  print(cntr)
  
  k <- k + 1
  
  recovery_trajectories_summary <- list.files(paste0("data/recovery_trajectories_summary/", cntr), full.names = TRUE) %>%
    map(read_csv) %>%
    bind_rows()
  
  recovery_trajectories_summary_out[[k]] <- recovery_trajectories_summary %>%
    group_by(since_disturbance) %>%
    summarize(treecover_rel_mean = mean(treecover_rel, na.rm = TRUE),
              treecover_rel_q05 = quantile(treecover_rel, 0.05, na.rm = TRUE),
              treecover_rel_q10 = quantile(treecover_rel, 0.10, na.rm = TRUE),
              treecover_rel_q25 = quantile(treecover_rel, 0.25, na.rm = TRUE),
              treecover_rel_q50 = quantile(treecover_rel, 0.50, na.rm = TRUE),
              treecover_rel_q75 = quantile(treecover_rel, 0.75, na.rm = TRUE),
              treecover_rel_q90 = quantile(treecover_rel, 0.90, na.rm = TRUE),
              treecover_rel_q95 = quantile(treecover_rel, 0.95, na.rm = TRUE)) %>%
    mutate(country = cntr)
  
}

recovery_trajectories_summary_country <- recovery_trajectories_summary_out  %>%
  bind_rows()

save(recovery_trajectories_summary_country, file = "temp/recovery_trajectories_summary_country.RData")
#load(file = "temp/recovery_trajectories_summary_country.RData")

# ggplot() +
#   geom_ribbon(data = recovery_trajectories_summary_country,
#               aes(x = since_disturbance, ymin = treecover_rel_q10, ymax = treecover_rel_q90, fill = country), alpha = 0.23) +
#   geom_ribbon(data = recovery_trajectories_summary_country,
#               aes(x = since_disturbance, ymin = treecover_rel_q25, ymax = treecover_rel_q75, fill = country), alpha = 0.23) +
#   geom_line(data = recovery_trajectories_summary_country,
#             aes(x = since_disturbance, y = treecover_rel_q50, col = country)) +
#   theme(legend.position = "none") +
#   facet_wrap(~country, scales = "free_y") +
#   geom_hline(yintercept = 1, linetype = "dashed")

# Recovery analysis -------------------------------------------------------

recovery_interval_collector <- c()
recovery_rate_collector <- c()

for (i in sort(cntrs)) {
  
  print(paste0("Finished ", round(mean(i > sort(cntrs)) * 100, 2), " %"))
  
  d <- recovery_trajectories_summary_country %>% filter(country == i)
  
  fit <- tryCatch(nlsLM(treecover_rel_mean ~ N + M / (1 + exp( -k * (since_disturbance - x0))),
                        data = d,
                        start = list(M = 1, k = 0.5, x0 = 15),
                        control = list(maxiter = 150)),
                  error = function(err) return(NA))
  
  if (is.na(fit)) {
    recovery_interval_collector <- c(recovery_interval_collector, NA)
    recovery_rate_collector <- c(recovery_rate_collector, NA)
  } else {
    p <- predict(fit, newdata = data.frame(since_disturbance = 1:1000))
    p <- which(p > 0.99)[1]
    recovery_interval_collector <- c(recovery_interval_collector,
                                     ifelse(is.na(p), 1001, p))
    recovery_rate_collector <- c(recovery_rate_collector, predict(fit, newdata = data.frame(since_disturbance = 30)))
  }
  
  
  
  
}

recovery_rates <- data.frame(country = cntrs,
                             recovery_interval = recovery_interval_collector,
                             recovery_rate = recovery_rate_collector) %>%
  as_tibble() %>%
  mutate(recovery_interval = ifelse(recovery_interval < 10, 10, recovery_interval))

# Disturbance intervals ---------------------------------------------------

disturbance_regime_indicators <- read_csv("data/disturbance_regime_senf_and_seidl_2021_nature_sustainability.csv")

grid <- read_sf("/data/Public/Projects/DisturbanceMappingEurope/mapping/data/gis/referencegrid/hexagon_50km.shp")
grid <- grid %>% dplyr::rename(gridid = id)

countries_sf <- read_sf("data/admin/countries_europe_simplyfied.shp")

gridid_country <- st_intersection(grid %>% filter(gridid %in% disturbance_regime_indicators$gridid), st_buffer(countries_sf, 0))

disturbance_intervals_country <- disturbance_regime_indicators %>%
  left_join(gridid_country %>% 
              st_drop_geometry() %>%
              dplyr::select(gridid, country = COUNTRY)) %>%
  group_by(country) %>%
  summarize(forest_ha = sum(forest_ha),
            disturbance_ha = sum(area_mean)) %>%
  mutate(disturbance_interval = forest_ha / disturbance_ha) %>%
  mutate(country = case_when(
    country == "The Former Yugoslav Republic of Macedonia" ~ "macedonia",
    country == "United Kingdom" ~ "unitedkingdom",
    country == "Bosnia and Herzegovina" ~ "bosniaherzegovina",
    country == "Czech Republic" ~ "czechia",
    TRUE ~ country
  )) %>%
  mutate(country = tolower(country))
  

# Country-scale analysis --------------------------------------------------

recovery_rates %>%
  left_join(disturbance_intervals_country) %>%
  mutate(resilience = disturbance_interval / recovery_interval) %>%
  dplyr::select(country, recovery_interval, disturbance_interval, resilience) %>%
  gather(key = interval, value = value, -country) %>%
  filter(interval == "resilience") %>%
  ggplot(., aes(x = reorder(country, value, max), y = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_wrap(~interval, scales = "free")

recovery_rates %>%
  left_join(disturbance_intervals_country) %>%
  mutate(resilience = disturbance_interval / recovery_interval) %>%
  dplyr::select(country, recovery_interval, disturbance_interval, resilience) %>% View(.)
  write.csv(., "results/table01.csv")
