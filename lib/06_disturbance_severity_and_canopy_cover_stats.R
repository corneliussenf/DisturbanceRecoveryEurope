
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggpubr)

# Load data and create summary file -------------------

cntrs <- list.files("data/recovery_trajectories_summary/")

recovery_trajectories_summary_samples_tmp <- vector("list", length(cntrs))

k <- 0

for (cntr in cntrs) {
  
  print(cntr)
  
  k <- k + 1
  
  recovery_trajectories_summary <- list.files(paste0("data/recovery_trajectories_summary/", cntr), full.names = TRUE) %>%
    map(read_csv) %>%
    bind_rows()
  
  patch_ids <- unique(recovery_trajectories_summary$disturbance_patch)
  patches_sample <- sample(patch_ids, round(length(patch_ids) * 0.01, 0))
  
  recovery_trajectories_summary_samples_tmp[[k]] <- recovery_trajectories_summary %>%
    filter(disturbance_patch %in% patches_sample)
  
}

recovery_trajectories_summary_samples <- recovery_trajectories_summary_samples_tmp %>%
  bind_rows()

save(recovery_trajectories_summary_samples, file = "temp/recovery_trajectories_summary_patch.RData")

# Calculate disturbance severity ------------------------------------------

load(file = "temp/recovery_trajectories_summary_patch.RData")

severity_summary <- recovery_trajectories_summary_samples %>%
  group_by(country, disturbance_patch, disturbance_year) %>%
  summarize(severity = 1 - (min(treecover, na.rm = TRUE) / unique(treecover_reference, na.rm = TRUE))) %>%
  ungroup()

p <- ggplot(severity_summary %>% filter(severity > -0.025), aes(x = severity * 100)) +
  geom_histogram(fill = "#BBBBBB") +
  labs(x = "Change in canopy cover during disturbance") +
  theme_linedraw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Canopy loss (%)", y = "Count", title = "Disturbance severity") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.grid = element_blank(),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.title = element_text(size = 6),
        legend.background = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 9))

ggsave("figures/SI_severity.pdf", p, width = 3.5, height = 3.5)

severity_summary$severity[severity_summary$severity < 0] <- 0

quantile(severity_summary$severity, seq(0, 1, 0.05), na.rm = TRUE)

mean(severity_summary$severity, na.rm = TRUE)
min(severity_summary$severity, na.rm = TRUE)
max(severity_summary$severity, na.rm = TRUE)

# Calculate canopy cover --------------------------------------------------

dat_ref <- readxl::read_excel("data/references/references.xlsx", col_names = TRUE, na = "NA")
dat_ref <- dat_ref[, 1:6]
dat_ref$gridid <- dat_ref$pixelid
dat_ref$pixelid <- NULL
dat_ref$country <- tolower(dat_ref$country)

mean(dat_ref$forest[dat_ref$forest > 10], na.rm = TRUE)
median(dat_ref$forest[dat_ref$forest > 10], na.rm = TRUE)
sd(dat_ref$forest[dat_ref$forest > 10], na.rm = TRUE)
range(dat_ref$forest[dat_ref$forest > 10], na.rm = TRUE)

hist(dat_ref$forest)
