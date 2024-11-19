# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 2. Load benthic cover data ----

load("data/09_misc/data-benthic.RData")

# 3. Load and combine predictors ----

## 3.1 Add territory for each site ----

data_eez <- st_read("data/01_background-shp/03_eez/data_eez.shp") %>% 
  select(TERRITORY1) %>% 
  rename(territory = TERRITORY1) %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

data_predictors <- st_read("data/04_site-coords/site-coords_all.shp") %>% 
  st_intersection(., data_eez) %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  mutate(site_id = as.numeric(site_id),
         year = 2000) %>% 
  tidyr::complete(year = seq(1980, 2023), nesting(site_id, type, territory, decimalLongitude, decimalLatitude))

## 3.2 Estimate human population for missing years ----

### 3.2.1 Load the data ----

pred_human_pop <- read.csv("data/10_predictors/pred_human-pop.csv") %>% 
  rename(year = system.index) %>% 
  mutate(year = as.numeric(str_split_fixed(year, "_", 9)[,6]))

### 3.2.2 Create the function ----

extract_coeff <- function(data){
  
  model <- lm(pred_population ~ year, data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}

### 3.2.3 Map over the function ----

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

## 3.3 Add other predictors ----

data_predictors <- read.csv("data/10_predictors/pred_elevation.csv") %>% 
  mutate(pred_elevation = replace_na(pred_elevation, 0)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_land.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_reef-extent.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_chla_mean.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_chla_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_gravity.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_enso.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_mean.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_mean_y1 = lag(pred_sst_mean, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_max_y1 = lag(pred_sst_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_min.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_skewness.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_dhw_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_dhw_max_y1 = lag(pred_dhw_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_cyclones.csv") %>% 
  left_join(data_predictors, .) %>% 
  mutate(across(c(wind_speed_y5, nb_cyclones, nb_cyclones_y5), ~replace_na(.x, 0)))

# 3.4 Round values of predictors ----

data_predictors <- data_predictors %>% 
  # Change unit for SST (°C)
  mutate(across(c(pred_sst_sd, pred_sst_max, 
                  pred_sst_mean, pred_sst_min,
                  pred_sst_max_y1, pred_sst_mean_y1), ~.x/100)) %>%
  # Round to 3 digits
  mutate(across(c(pred_elevation, pred_reefextent, pred_land,
                  pred_enso, pred_chla_mean, pred_chla_sd),
                ~ round(.x, digits = 3))) %>% 
  # Round to 2 digits
  mutate(across(c(pred_sst_sd, pred_sst_skewness,
                  pred_sst_max, pred_sst_mean,
                  pred_sst_min, pred_dhw_max, pred_dhw_max_y1,
                  pred_sst_max_y1, pred_sst_mean_y1),
                ~ round(.x, digits = 2)))

# 4. Feature selection (remove correlated predictors) ----

## 4.1 Find correlation coefficients between predictors ----

data_correlation <- data_predictors %>% 
  select(-site_id, -year, -type, -territory) %>% 
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
  select(-pred_sst_min)

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

save(data_predictors_pred, file = "data/11_model-data/data_predictors_pred.RData")

# 6. Summarize data and add predictors ----

data_site_coords_obs <- st_read("data/04_site-coords/site-coords_obs.shp") %>% 
  mutate(site_id = as.numeric(site_id),
         decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

## 6.1 Major hard coral families ----

data_benthic_hc <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  filter(family %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
  mutate(category = family) %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  # This is the case for datasets "0011", "0012", "0013", "0014", and "0043" at least
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Remove values greater than 100 (unlikely but included to avoid any issues later)
  filter(measurementValue <= 100) %>% 
  # 4. Remove useless variables
  select(-higherGeography, -country, -locality, -habitat, -eventDate) %>% 
  # 5. Convert to factors
  mutate_if(is.character, factor) %>% 
  # 6. Add site_id and type (to join on step 7)
  left_join(., data_site_coords_obs) %>% 
  # 7. Add predictors
  left_join(., data_predictors %>%
              filter(type == "obs") %>% 
              # Remove lat and long because GEE slightly modify these, which break the join
              select(-decimalLongitude, -decimalLatitude),
            by = c("site_id", "year", "territory", "type")) %>% 
  select(-site_id, -type)

## 6.2 Major benthic categories ----

data_benthic <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  # This is the case for datasets "0011", "0012", "0013", "0014", and "0043" at least
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Remove values greater than 100 (unlikely but included to avoid any issues later)
  filter(measurementValue <= 100) %>% 
  # 4. Remove useless variables
  select(-higherGeography, -country, -locality, -habitat, -eventDate) %>% 
  # 5. Convert to factors
  mutate_if(is.character, factor) %>% 
  # 6. Add site_id and type (to join on step 7)
  left_join(., data_site_coords_obs) %>% 
  # 7. Add predictors
  left_join(., data_predictors %>%
              filter(type == "obs") %>% 
              # Remove lat and long because GEE slightly modify these, which break the join
              select(-decimalLongitude, -decimalLatitude),
            by = c("site_id", "year", "territory", "type")) %>% 
  select(-site_id, -type) %>% 
  bind_rows(., data_benthic_hc)

## 6.3 Check the number of NA per variable ----

data_benthic_na <- data_benthic %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_benthic),
         percent = (na*100)/n)

## 6.4 Export the data ----

save(data_benthic, file = "data/11_model-data/data_benthic_prepared.RData")

# 7. Raw data time series ----

## 7.1 Add territories with no surveys ----

data_benthic <- expand.grid(territory = unique(data_eez$territory),
                         category = unique(data_benthic$category)) %>% 
  left_join(., data_benthic) %>% 
  mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                           category == "Coralline algae" ~ palette_second[3],
                           category == "Macroalgae" ~ palette_second[4],
                           category == "Turf algae" ~ palette_second[5],
                           category == "Acroporidae" ~ palette_first[1],
                           category == "Poritidae" ~ palette_first[2],
                           category == "Pocilloporidae" ~ palette_first[3]))

# 7. Plot raw data ----

## 7.2 Make the plots ----

### 7.2.1 Add plot id ----

data_benthic <- tibble(territory = sort(unique(data_benthic$territory)),
                       id = rep(1:5, each = 6)) %>% 
  left_join(., data_benthic) %>% 
  mutate(territory = str_replace_all(territory, c("Northern Mariana Islands" = "North. Mariana Isl.",
                                                  "Howland and Baker Islands" = "Howl. and Baker Isl.",
                                                  "Federated States of Micronesia" = "Fed. St. of Micronesia")))

### 7.2.2 Create the function ----

plot_raw_data <- function(id_i){
  
  plot_i <- data_benthic %>% 
    filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae"),
           id == id_i) %>% 
    ggplot(data = ., aes(x = year, y = measurementValue, color = color, fill = color)) +
    geom_point(show.legend = FALSE, alpha = 0.1) +
    stat_summary(geom = "point", fun = "mean", col = "black", size = 2, shape = 23) +
    scale_color_identity() +
    scale_fill_identity() +
    labs(x = "Year", y = "Cover (%)") +
    coord_cartesian(clip = "off") +
    facet_grid(territory~category, scales = "free") +
    theme_graph() +
    theme(strip.text = element_text(hjust = 0.5),
          axis.text = element_text(size = 12),
          strip.background = element_blank(),
          panel.spacing = unit(1, "lines")) +
    scale_x_continuous(limits = c(1985, 2025)) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100))
  
  ggsave(filename = paste0("figs/04_supp/01_data-explo/05_raw-data_major-cat_", letters[id_i],".png"),
         plot = plot_i, width = 10, height = 12, dpi = fig_resolution)
  
}

### 7.2.3 Map over the function ----

map(unique(data_benthic$id), ~plot_raw_data(id_i = .))
