# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 2. Load benthic cover data ----

load("data/02_misc/data-benthic.RData")

# 3. Load and combine predictors ----

## 3.1 Add area to each site ----

### 3.1.1 First assignation ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_benthic_coords <- st_read("data/03_site-coords/site-coords_all.shp") %>% 
  st_join(., data_area)

### 3.1.2 Second assignation ----

data_predictors <- data_benthic_coords %>% 
  filter(is.na(area)) %>% 
  select(-area) %>% 
  st_join(., st_buffer(data_area, 0.05)) %>% 
  bind_rows(data_benthic_coords %>% filter(!(is.na(area))), .)

### 3.1.3 Generate all years ----

data_predictors <- data_predictors %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  mutate(site_id = as.numeric(site_id),
         year = 2000) %>% 
  tidyr::complete(year = seq(1980, 2024), nesting(site_id, type, area, decimalLongitude, decimalLatitude))

## 3.2 Add territory to each site ----

data_eez <- st_read("data/01_maps/01_raw/02_eez/eez_v12.shp")

data_predictors <- st_read("data/03_site-coords/site-coords_all.shp") %>% 
  st_join(., data_eez) %>% 
  select(TERRITORY1, site_id, type) %>% 
  rename(territory = TERRITORY1) %>%
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"],
         site_id = as.numeric(site_id)) %>% 
  st_drop_geometry() %>% 
  left_join(data_predictors, .)

## 3.3 Estimate human population for missing years ----

### 3.3.1 Load the data ----

pred_human_pop <- read.csv("data/08_predictors/pred_human-pop.csv") %>% 
  rename(year = system.index) %>% 
  mutate(year = as.numeric(str_split_fixed(year, "_", 9)[,6]))

### 3.3.2 Create the function ----

extract_coeff <- function(data){
  
  model <- lm(pred_population ~ year, data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}

### 3.3.3 Map over the function ----

pred_human_pop <- pred_human_pop %>% 
  # Extract linear model coefficients
  group_by(site_id, type) %>% 
  group_modify(~extract_coeff(data = .x)) %>% 
  ungroup() %>% 
  left_join(pred_human_pop, .) %>% 
  # Estimate human population for all years between 2000 and 2023
  tidyr::complete(year = seq(2000, 2023), nesting(site_id, type, intercept, slope)) %>% 
  mutate(pred_population = (year*slope)+intercept) %>% 
  select(-intercept, -slope) %>% 
  mutate(pred_population = round(pred_population))

data_predictors <- left_join(data_predictors, pred_human_pop)

## 3.4 Add other predictors ----

data_predictors <- read.csv("data/08_predictors/pred_elevation.csv") %>% 
  mutate(pred_elevation = replace_na(pred_elevation, 0)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_land.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_reef-extent.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_chla_mean.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_chla_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_gravity.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_enso.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_mean.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_mean_y1 = lag(pred_sst_mean, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_max_y1 = lag(pred_sst_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_min.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_skewness.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_sst_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_dhw_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_dhw_max_y1 = lag(pred_dhw_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_ssta_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_ssta_mean.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/08_predictors/pred_cyclones.csv") %>% 
  left_join(data_predictors, .) %>% 
  mutate(across(c(windspeed_y5, nb_cyclones, nb_cyclones_y5), ~replace_na(.x, 0)))

# 3.5 Round values of predictors ----

data_predictors <- data_predictors %>% 
  # Change unit for SST (°C)
  mutate(across(c(pred_sst_sd, pred_sst_max, 
                  pred_sst_mean, pred_sst_min,
                  pred_sst_max_y1, pred_sst_mean_y1,
                  pred_ssta_mean, pred_ssta_max), ~.x/100)) %>%
  # Round to 3 digits
  mutate(across(c(pred_elevation, pred_reefextent, pred_land,
                  pred_enso, pred_chla_mean, pred_chla_sd),
                ~ round(.x, digits = 3))) %>% 
  # Round to 2 digits
  mutate(across(c(pred_sst_sd, pred_sst_skewness,
                  pred_sst_max, pred_sst_mean,
                  pred_sst_min,
                  #pred_dhw_max, pred_dhw_max_y1,
                  pred_sst_max_y1, pred_sst_mean_y1),
                ~ round(.x, digits = 2)))

# 4. Feature selection (remove correlated predictors) ----

## 4.1 Find correlation coefficients between predictors ----

data_correlation <- data_predictors %>% 
  select(-site_id, -year, -type, -area, -territory) %>% 
  cor(., use = "complete.obs") %>% 
  round(., 2) %>% 
  as_tibble(.) %>% 
  mutate(predictor_a = colnames(.)) %>% 
  pivot_longer(1:ncol(.)-1, names_to = "predictor_b", values_to = "coefficient") %>% 
  filter(predictor_a != predictor_b) %>% 
  arrange(coefficient) %>% 
  # Remove odd row number to remove duplicated pairs of predictors
  filter(row_number(.) %% 2 == 0)

## 4.2 Remove useless predictors based on correlation ----

data_predictors <- data_predictors %>% 
  select(-pred_sst_mean_y1)

# 5. Export predictors for data to predict ----

data_predictors_pred <- data_predictors %>% 
  filter(type == "pred") %>% 
  select(-type, -site_id) %>% 
  mutate(datasetID = NA,
         month = NA,
         day = NA,
         verbatimDepth = NA,
         parentEventID = NA,
         eventID = NA)

save(data_predictors_pred, file = "data/09_model-data/data_predictors_pred.RData")

# 6. Summarize data and add predictors ----

data_site_coords_obs <- st_read("data/03_site-coords/site-coords_obs.shp") %>% 
  mutate(site_id = as.numeric(site_id),
         decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

## 6.1 Major hard coral families ----

data_benthic_hc <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  filter(family %in% c("Acroporidae", "Merulinidae")) %>% 
  mutate(category = family) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Regenerate 0 values
  group_by(datasetID) %>% 
  complete(category,
           nesting(region, subregion, ecoregion, country, territory, area, locality,
                   habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                   year, month, day, eventDate),
           fill = list(measurementValue = 0)) %>%
  ungroup() %>%
  # 4. Remove values greater than 100 (unlikely but included to avoid any issues later)
  filter(measurementValue <= 100) %>% 
  # 5. Remove useless variables
  select(-region, -subregion, -ecoregion, -country, -locality, -habitat, -eventDate) %>% 
  # 6. Convert to factors
  mutate_if(is.character, factor) %>% 
  # 7. Add site_id and type (to join on step 7)
  left_join(., data_site_coords_obs) %>% 
  # 8. Add predictors
  left_join(., data_predictors %>%
              filter(type == "obs") %>% 
              # Remove lat and long because GEE slightly modify these, which break the join
              select(-decimalLongitude, -decimalLatitude, -territory),
            by = c("site_id", "year", "area", "type")) %>% 
  select(-site_id, -type)

## 6.2 Major benthic categories ----

data_benthic <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae", "Other fauna")) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Regenerate 0 values
  group_by(datasetID) %>% 
  complete(category,
           nesting(region, subregion, ecoregion, country, territory, area, locality,
                   habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                   year, month, day, eventDate),
           fill = list(measurementValue = 0)) %>%
  ungroup() %>%
  # 4. Remove values greater than 100 (unlikely but included to avoid any issues later)
  filter(measurementValue <= 100) %>% 
  # 5. Remove useless variables
  select(-region, -subregion, -ecoregion, -country, -locality, -habitat, -eventDate) %>% 
  # 6. Convert to factors
  mutate_if(is.character, factor) %>% 
  # 7. Add site_id and type (to join on step 7)
  left_join(., data_site_coords_obs) %>% 
  # 8. Add predictors
  left_join(., data_predictors %>%
              filter(type == "obs") %>% 
              # Remove lat and long because GEE slightly modify these, which break the join
              select(-decimalLongitude, -decimalLatitude, -territory),
            by = c("site_id", "year", "area", "type")) %>% 
  select(-site_id, -type) %>% 
  bind_rows(., data_benthic_hc)

## 6.3 Check the number of NA per variable ----

data_benthic_na <- data_benthic %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_benthic),
         percent = (na*100)/n)

## 6.4 Export the data ----

save(data_benthic, file = "data/09_model-data/data_benthic_prepared.RData")
