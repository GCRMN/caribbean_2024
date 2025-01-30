# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(scales)
library(zoo)
library(Kendall)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/combine_model_data.R")
source("code/function/extract_mannkendall.R")
source("code/function/plot_vimp.R")
source("code/function/plot_pdp.R")
source("code/function/plot_trends.R")
source("code/function/plot_residuals.R")
source("code/function/plot_pred_obs.R")
source("code/function/combine_plot_trends.R")

theme_set(theme_graph())

# 3. Data preparation ----

## 3.1 Combine model results ----

model_results <- combine_model_data(model = "xgb")

## 3.2 Confidence intervals ----

raw_trends <- model_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, region, area, year, color, text_title) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.05),
            lower_ci_80 = quantile(cover, 0.20),
            upper_ci_95 = quantile(cover, 0.95),
            upper_ci_80 = quantile(cover, 0.80)) %>% 
  ungroup()

## 3.3 Long-term average ----

long_term_average <- raw_trends %>% 
  group_by(category, area) %>% 
  summarise(across(c(mean, lower_ci_80, upper_ci_80), ~mean(.x, na.rm = TRUE))) %>% 
  ungroup()

## 3.4 Long-term trend ----

long_term_trend <- raw_trends %>% 
  group_by(category, area) %>% 
  group_modify(~extract_mannkendall(data = .x, var_y = "mean")) %>% 
  ungroup()

## 3.5 Add "obs_data" variable ----

load("data/09_model-data/data_benthic_prepared.RData")

data_benthic_obs <- data_benthic %>% 
  select(year, area, category) %>% 
  distinct() %>% 
  mutate(region = "Caribbean",
         data_obs = 1)

data_benthic_obs <- data_benthic %>% 
  select(year, category) %>% 
  distinct() %>% 
  mutate(region = "Caribbean",
         area = "All",
         data_obs = 1) %>% 
  bind_rows(., data_benthic_obs)

raw_trends <- left_join(raw_trends, data_benthic_obs) %>% 
  mutate(data_obs = replace_na(data_obs, 0))

## 3.6 Smooth moving average ----

smoothed_trends <- raw_trends %>% 
  group_by(category, region, area, color, text_title) %>% 
  # Two years moving average
  mutate(across(c("mean", "lower_ci_95", "lower_ci_80", "upper_ci_95", "upper_ci_80"),
                ~rollmean(.x, k = 2, fill = NA, align = "center"))) %>% 
  ungroup() %>% 
  # Add first year data
  filter(year != 1980) %>% 
  bind_rows(., raw_trends %>% 
              filter(year == 1980)) %>% 
  arrange(category, region, area, year)

## 3.7 Combine into a list ----

data_trends <- lst(smoothed_trends, raw_trends, long_term_average, long_term_trend)

rm(smoothed_trends, raw_trends, long_term_average,
   long_term_trend, data_benthic, data_benthic_obs)

## 3.8 Export the data ----



# 4. Model evaluation ----

## 4.1 Hyper-parameters ----

model_results$model_description %>% 
  select(-model, -color, -text_title, -grid_size)

## 4.2 Performance ----

model_results$model_performance %>% 
  group_by(category) %>% 
  summarise(across(c(rmse, rsq), ~mean(.x)))

## 4.3 Predicted vs observed ----

plot_pred_obs(all = TRUE)

map(unique(model_results$model_pred_obs$category), ~plot_pred_obs(category_i = .))

## 4.4 Residuals ----

plot_residuals(all = TRUE)

map(unique(tuning_results$result_pred_obs$category), ~plot_residuals(category_i = .))

## 4.5 VIMP ----

### 4.5.1 Transform data ----

data_imp_summary <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  group_by(predictor, category, color) %>% 
  summarise(mean = mean(importance),
            lower_ci_95 = quantile(importance, 0.05),
            lower_ci_80 = quantile(importance, 0.10),
            upper_ci_95 = quantile(importance, 0.95),
            upper_ci_80 = quantile(importance, 0.80)) %>% 
  ungroup() %>% 
  group_by(category, color) %>% 
  arrange(desc(mean)) %>% 
  slice_head(n = 25) %>% 
  ungroup()

data_imp_raw <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  left_join(data_imp_summary, .) %>% 
  group_by(category)

### 4.5.2 Map over the function ----

map(unique(data_imp_summary$category), ~plot_vimp(category_i = .))

rm(data_imp_raw)

## 4.6 PDP ----

### 4.6.1 Calculate confidence intervals ----

data_pdp <- model_results$result_pdp %>% 
  group_by(category, predictor, x, color, text_title) %>% 
  summarise(mean = mean(y_pred, na.rm = TRUE),
            upper_ci = quantile(y_pred, probs = 0.975),
            lower_ci = quantile(y_pred, probs = 0.025)) %>% 
  ungroup()

### 4.6.2 Map over the function ----

map(unique(data_pdp$category), ~plot_pdp(category_i = .x))

# 5. Temporal trends ----

## 5.1 For major benthic categories ----

map(unique(data_trends$smoothed_trends$area),
    ~combine_plot_trends(area_i = ., categ_type = "categories"))

## 5.2 For hard coral families ----

map(unique(data_trends$smoothed_trends$area),
    ~combine_plot_trends(area_i = ., categ_type = "families"))

## 5.3 Raw data (for writing) ----

if(FALSE){
  
  A <- data_trends %>% filter(territory == "All" & category == "Acroporidae") %>% select(-upper_ci_95, -lower_ci_95)
  #A <- data_trends %>% filter(territory == "All" & category == "Acroporidae") %>% select(-upper_ci_95, -lower_ci_95) %>% 
  filter(year >= 1987 & year <= 1999) %>% summarise(mean = mean(mean))
  
  A <- data_trends %>% filter(territory == "Guadeloupe" & category == "Turf algae") %>% select(-upper_ci_95, -lower_ci_95) %>% 
    summarise(mean = mean(mean),
              lower_ci_80 = mean(lower_ci_80),
              upper_ci_80 = mean(upper_ci_80))
  
}

## 5.4 Figure for Executive Summary ----

data_ex_summ <- data_trends$raw_trends %>% 
  filter(area == "All")

data_ex_summ <- data_ex_summ %>% 
  group_by(year) %>% 
  summarise(mean = sum(mean)) %>% 
  ungroup() %>% 
  # Generate the others category (difference between 100 and sum of all categories)
  mutate(mean = 100 - mean, 
         category = "Others",
         color = "lightgrey") %>% 
  bind_rows(data_ex_summ, .)

ggplot(data = data_ex_summ, aes(x = year, y = mean, fill = category)) +
  geom_area() +
  scale_fill_manual(values = unique(data_ex_summ$color)) +
  theme_graph() +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/00_misc/benthic-trends.png", width = 6, height = 4, dpi = fig_resolution)















## Map of predicted values across the region ----


library(sf)
sf_use_s2(FALSE)

# 1. Load data

## 1.1 Predictions per site and year

load("data/10_model-output/model_results_hard-coral_xgb_test.RData")

U <- model_results$results_predicted %>% 
  drop_na(year) %>% # Due to not exported results (to save memory)
  group_by(year, decimalLatitude, decimalLongitude) %>% 
  summarise(measurementValuepred = mean(measurementValuepred)) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 1.2 Crop of the region

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

## 1.3 Background map

data_land_ne <- read_sf("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_land_ne <- st_intersection(data_land_ne, data_crop)

# 2. Create the grid

data_grid <- st_make_grid(data_crop, n = 200, crs = 4326) %>% 
  st_as_sf() %>% 
  mutate(cell_id = 1:nrow(.))

# 3. Summarize sites values per grid cell

test <- st_join(U, data_grid) %>% 
  group_by(year, cell_id) %>% 
  summarise(cover_pred = mean(measurementValuepred)) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  left_join(., data_grid) %>% 
  st_as_sf()

# 4. Dataviz

## 4.1 Using the grid

A <- ggplot() +
  geom_sf(data = test, aes(fill = cover_pred), color = NA) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~year, ncol = 10) +
  geom_sf(data = data_land_ne, linewidth = 0.1) +
  theme(strip.background = element_rect(fill = NA, linewidth = 0)) +
  coord_sf(xlim = c(-105, -50), ylim = c(6, 38), expand = FALSE)

ggsave("figs/06_additional/map_predictions.png", plot = A, width = 25, height = 15)

## 4.2 Using the sites

A <- ggplot() +
  geom_sf(data = U, aes(color = measurementValuepred), alpha = 0.8) +
  scale_color_continuous(type = "viridis") +
  facet_wrap(~year) +
  geom_sf(data = data_land_ne, linewidth = 0.1) +
  coord_sf(xlim = c(-105, -50), ylim = c(6, 38), expand = FALSE)

ggsave("test.png", plot = A)




#### PDP - Colorer par catégorie plus tard

model_results$result_pdp %>% 
  group_by(predictor, x) %>% 
  summarise(mean = mean(y_pred, na.rm = TRUE),
            upper_ci = quantile(y_pred, probs = 0.975),
            lower_ci = quantile(y_pred, probs = 0.025)) %>% 
  ungroup() %>% 
  ggplot(data = .) +
    geom_ribbon(aes(x = x, ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
    geom_line(aes(x = x, y = mean)) +
    facet_grid(~predictor, scales = "free") +
    theme(strip.background = element_rect(fill = NA, linewidth = 0))


