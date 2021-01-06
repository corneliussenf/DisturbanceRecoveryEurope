
# Global libraries --------------------------------------------------------

library(tidyverse)
library(raster)
library(randomForest)

# Create cal/val data -----------------------------------------------------

### Load extracted  Landsat data

dat_lt <- list.files("extraction", "csv", full.names = TRUE) %>%
  map(read_csv) %>%
  bind_rows(.)

### Caclulate vegetation indices

dat_lt <- dat_lt %>%
  spread(key = band, value = value) %>%
  mutate(ndvi = (B4 - B3) / (B4 + B3),
         evi = 2.5 * ((B4 - B3) / (B4 + 6 * B3 - 7.5 * B1 + 1)),
         savi = ((B4 - B3) / (B4 + B3 + 0.5)) * (1.5),
         msavi = (2 * B4 + 1 - sqrt ((2 * B4 + 1)^2 - 8 * (B4 - B3))) / 2,
         ndmi = (B4 - B5) / (B4 + B5),
         nbr = (B4 - B7) / (B4 + B7),
         nbr2 = (B5 - B7) / (B5 + B7)) %>%
  gather(key = band, value = value, -(pixelid:country)) %>%
  group_by(gridid, year, band, country) %>%
  summarise(mean = mean(value, na.rm = TRUE), # Calculate mean/sd over the 5*5 pixel grid used for training
            sd = sd(value, na.rm = TRUE))

### Load reference data and join to Landsat data

dat_ref <- readxl::read_excel("data/references/references.xlsx", col_names = TRUE, na = "NA")
dat_ref <- dat_ref[, 1:6]
dat_ref$gridid <- dat_ref$pixelid
dat_ref$pixelid <- NULL
dat_ref$country <- tolower(dat_ref$country)

dat <- dat_lt %>%
  left_join(dat_ref, by = c("gridid", "year", "country")) %>%
  na.omit()

### Find and replace duplicate cases

# This can happen if two high-res images were available in one year where a disturbance occured
# Hence, there will be two entries for one year with  different forest covers
# As it is not clear which one correspinds to the Landsat observations, we remove both

doubles <- dat %>% group_by(gridid, year, band, country) %>% summarise(n = n())
doubles <- doubles %>%
  filter(n > 1) %>%
  group_by(gridid, country) %>%
  summarize() %>%
  ungroup()

dat <- dat %>%
  anti_join(doubles)

### Spread to wide-format

dat_model <- dat %>%
  data.table::setDT(.) %>%
  data.table::dcast(gridid + year + country + forest + broadleaved + coniferous ~ band, 
                    value.var = c("mean", "sd")) %>%
  data.table::setDF(.)

### Remove NA's and infinite values (might be caused by clouds, cloud shadows, snow, etc.)

nrow_before <- nrow(dat_model)

dat_model <- dat_model %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter_all(all_vars(!is.infinite(.)))

nrow_after <- nrow(dat_model)

print(paste0("Removed ", nrow_before - nrow_after, " rows due to NA and/or Inf values!"))

# ### Create response classes
# 
# dat_model <- dat_model %>%
#   mutate(forest_density = case_when(forest < 10 ~ "Noforest",
#                                     forest >= 10 & forest < 40 ~ "open",
#                                     forest >= 40 ~ "closed"),
#          forest_type = case_when((coniferous > broadleaved) & forest >= 10 ~ "-coniferous",
#                                  (broadleaved >= coniferous) & forest >= 10 ~ "-broadleaved",
#                                  forest < 10 ~ ""),
#          response_class = paste0(forest_density, forest_type))
# 
# dat_model$response_class <- as.factor(dat_model$response_class)
#
# table(dat_model$response_class)

### Save to disc

dat_model$country <- as.factor(dat_model$country)

write_csv(dat_model, "temp/dat_model.csv")

# Cross-validate model performance ----------------------------------------

dat_model <- read_csv("temp/dat_model.csv")

### Create folds for 10-fold corss validation

dat_model_folds <- caret::createFolds(dat_model$forest)

### Function for training the models and returning predicted vs. observed df

cross_val_rf <- function(cal, val) {
  
  fit_forest <- randomForest(x = cal %>% dplyr::select(mean_B1:mean_savi, country),
                             y = cal$forest)
  
  cal$predict_forest <- predict(fit_forest)
  
  fit_coniferous <- randomForest(x = cal %>% dplyr::select(mean_B1:mean_savi, predict_forest, country),
                                 y = cal$coniferous)
  
  cal$predict_coniferous <- predict(fit_coniferous)
  
  fit_broadlaeaved <- randomForest(x = cal %>% dplyr::select(mean_B1:mean_savi, predict_forest, predict_coniferous, country),
                                   y = cal$broadleaved)
  
  val$predict_forest <- predict(fit_forest, newdata = val)
  val$predict_coniferous <- predict(fit_coniferous, newdata = val)
  val$predict_broadleaved <- predict(fit_broadlaeaved, newdata = val)
  
  return(val)
  
}

cross_val_rf_2 <- function(cal, val) {
  
  fit_forest <- randomForest(x = cal %>% dplyr::select(mean_B1:mean_savi, country),
                             y = cal$forest)
  
  val$predicted <- predict(fit_forest, newdata = val)
  
  return(val)
  
}

### Run cross-validation

validation <- vector("list", 10)

for (i in 1:10) {
  
  print(i)
  
  folds <- dat_model_folds[[i]]
  
  calibration <- dat_model[-folds, ]
  validation_tmp <- dat_model[folds, ]
  
  validation[[i]] <- cross_val_rf_2(cal = calibration, val = validation_tmp)
  
}

validation <- validation %>% set_names(1:10) %>% bind_rows(.id = "fold")

### Assess model performance 

performance <- validation %>%
  #filter(!(forest == 0 & predict_forest == 0)) %>%
  summarize(r2_forest = cor(forest, .$predicted)^2,
            #r2_coniferous = cor(coniferous, predict_coniferous)^2,
            #r2_broadleaved = cor(broadleaved, predict_broadleaved)^2,
            rmse_forest = sqrt(mean((forest - predicted)^2)),
            #rmse_coniferous = sqrt(mean((coniferous - predict_coniferous)^2)),
            #rmse_broadleaved = sqrt(mean((broadleaved - predict_broadleaved)^2)),
            nrmse_forest = rmse_forest / mean(forest)
            #nrmse_coniferous = rmse_coniferous / mean(coniferous),
            #nrmse_broadleaved = rmse_broadleaved / mean(broadleaved)
            ) %>%
  gather() %>%
  separate("key", c("measure", "level"), "\\_") %>%
  spread(key = measure, value  = value) %>%
  mutate(level = str_to_title(level))
  
p <- validation %>%
  dplyr::select(observed_forest = forest,
                #observed_broadleaved = broadleaved,
                #observed_coniferous = coniferous,
                predicted_forest = predicted,
                #predicted_broadleaved = predict_broadleaved,
                #predicted_coniferous = predict_coniferous,
                gridid, year, country) %>%
  gather(key = key, value = value, -gridid, -year, -country) %>% 
  separate("key", c("key", "level"), "_") %>% 
  spread(key = key, value = value) %>%
  mutate(level = stringr::str_to_title(level)) %>%
  ggplot(.) +
  geom_point(aes(x = observed, y = predicted), col = "grey", alpha = 0.15, shape = 16) +
  geom_abline(intercept = 0, slope = 1, col = "black", linetype = "dashed") +
  #facet_wrap(~level, ncol = 3) +
  geom_text(data = performance, x = 10, y = 97, size = 2.5, 
            aes(label = paste0("R^{2}==", round(r2, 2))), parse = TRUE) +
  geom_text(data = performance, x = 10, y = 87, size = 2.5, 
            aes(label = paste0("RMSE==", round(rmse, 2))), parse = TRUE) +
  geom_text(data = performance, x = 10, y = 77, size = 2.5, 
            aes(label = paste0("nRMSE==", round(nrmse * 100, 0),  "*'%\'")), parse = TRUE) +
  theme_classic() +
  scale_x_continuous(breaks = c(10, 30, 50, 70, 90)) +
  scale_y_continuous(breaks = c(10, 30, 50, 70, 90)) +
  theme(legend.position = "non",
        strip.background = element_blank(),
        panel.border = element_rect(fill = NA, size = 0.75),
        panel.spacing = unit(0, "lines"),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) +
  labs(x = "Observed forest cover (%)", y = "Predicted forest cover (%)")

ggsave("results/validation.pdf", p, width = 3.5, height = 3.5)

p

### Check whether broadleaved + coniferous sums up to forest cover

# performance_consistency <- validation %>%
#   summarize(r2 = cor(broadleaved + coniferous, forest)^2,
#             rmse = sqrt(mean(((broadleaved + coniferous) - forest)^2)),
#             nrmse = rmse / mean(forest))

# Train final model -------------------------------------------------------

# fit_classes <- randomForest(x = dat_model %>% dplyr::select(mean_B1:mean_savi, country),
#                             y = dat_model$response_class)

fit_forest <- randomForest(x = dat_model %>% dplyr::select(mean_B1:mean_savi, country),
                           y = dat_model$forest)

# dat_model$predict_forest <- predict(fit_forest)
# 
# fit_coniferous <- randomForest(x = dat_model %>% dplyr::select(mean_B1:mean_savi, predict_forest, country),
#                                y = dat_model$coniferous)
# 
# dat_model$predict_coniferous <- predict(fit_coniferous)
# 
# fit_broadlaeaved <- randomForest(x = dat_model %>% dplyr::select(mean_B1:mean_savi, predict_forest, predict_coniferous, country),
#                                  y = dat_model$broadleaved)

save(fit_forest, file = "results/models.RData")

# save(fit_classes, file = "results/models.RData")
