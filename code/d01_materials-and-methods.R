# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(patchwork)
library(ggtext)
library(ggspatial) # For annotation_scale function

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/extract_coeff.R")

# 3. Cyclones ----

## 3.1. Load data ----

### 3.1.1 Coral reef distribution ----

data_reef <- st_read("data/01_maps/02_clean/02_reefs/reefs.shp") %>% 
  filter(area == "Guadeloupe")

### 3.1.2 Land ----

data_land <- st_read("data/01_maps/02_clean/05_princeton/land.shp") %>% 
  filter(area == "Guadeloupe")

### 3.1.3 Coral reef distribution 100 km buffer ----

data_reef_buffer <- st_read("data/01_maps/02_clean/02_reefs/reefs_buffer_100.shp") %>% 
  filter(area == "Guadeloupe")

### 3.1.4 Cyclones lines ----

data_zone <- tibble(lon = c(-64, -59),
                    lat = c(14, 18)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

load("data/07_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = 4326) %>% 
  filter(ts_id %in% c("2022257N16312", "1993227N14301"))

data_ts_lines <- st_intersection(data_ts_lines, data_zone)

### 3.1.5 Cyclones points ----

load("data/07_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = 4326) %>% 
  filter(ts_id %in% c("2022257N16312", "1993227N14301"))

data_ts_points <- st_intersection(data_ts_points, data_zone)

## 3.2. Make the plots ----

### 3.2.1 Plot A ----

plot_a <- ggplot() +
  geom_sf(data = data_reef_buffer, fill = "#f1a9a0", alpha = 0.25) +
  geom_sf(data = data_reef, col = "#d64541") +
  geom_sf(data = data_land, fill = "lightgrey") +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph, hjust = 0.5),
        panel.background = element_rect(colour = NA, fill = NA, linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**1.** Create 100 km coral reef buffer") +
  coord_sf(xlim = c(as.numeric(st_bbox(data_zone)$xmin), as.numeric(st_bbox(data_zone)$xmax)),
           ylim = c(as.numeric(st_bbox(data_zone)$ymin), as.numeric(st_bbox(data_zone)$ymax)))

### 3.2.2 Plot B ----

plot_b <- ggplot() +
  geom_sf(data = data_reef_buffer, fill = "lightgrey", alpha = 0.25) +
  geom_sf(data = data_reef, col = "#d64541") +
  geom_sf(data = data_land, fill = "lightgrey") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "2022257N16312"), col = "#446cb3") +
  geom_sf(data = data_ts_points %>% filter(ts_id == "2022257N16312"), shape = 21, 
          col = "black", fill = "#446cb3", size = 3) +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "1993227N14301")) +
  geom_sf(data = data_ts_points %>% filter(ts_id == "1993227N14301"), shape = 21, fill = "white", size = 3) +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph, hjust = 0.5),
        panel.background = element_rect(colour = NA, fill = NA, linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**2.** Filter cyclones crossing the buffer") +
  coord_sf(xlim = c(as.numeric(st_bbox(data_zone)$xmin), as.numeric(st_bbox(data_zone)$xmax)),
           ylim = c(as.numeric(st_bbox(data_zone)$ymin), as.numeric(st_bbox(data_zone)$ymax)))

### 3.2.3 Plot C ----

plot_c <- ggplot() +
  geom_sf(data = data_reef_buffer, fill = "lightgrey", alpha = 0.25) +
  geom_sf(data = data_reef, col = "#d64541") +
  geom_sf(data = data_land, fill = "lightgrey") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "2022257N16312")) +
  geom_sf(data = data_ts_points %>% filter(ts_id == "2022257N16312"), shape = 21, 
          col = "black", fill = "white", size = 3) +
  geom_sf(data = data_ts_points %>% 
            filter(ts_id == "2022257N16312") %>% 
            filter(row_number() == 4), shape = 21, fill = "#446cb3", size = 3) +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph, hjust = 0.5),
        panel.background = element_rect(colour = NA, fill = NA, linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**3.** Extract cyclone's distance and wind speed") +
  coord_sf(xlim = c(as.numeric(st_bbox(data_zone)$xmin), as.numeric(st_bbox(data_zone)$xmax)),
           ylim = c(as.numeric(st_bbox(data_zone)$ymin), as.numeric(st_bbox(data_zone)$ymax)))

## 3.3. Combine the plots ----

plot_a + plot_b + plot_c + plot_layout(ncol = 3)

## 3.4. Export the plot ----

ggsave("figs/04_methods/fig-1.png", height = 4.5, width = 15, dpi = fig_resolution)

# 4. SST change and warming rate ----

## 4.1. Load data ----

load("data/02_misc/data-sst_raw.RData")

data_warming <- data_sst %>% 
  # Convert date as numeric
  mutate(date = as.numeric(as_date(date))) %>% 
  # Extract linear model coefficients
  group_by(area) %>% 
  group_modify(~extract_coeff(data = .x, var_y = "sst", var_x = "date")) %>% 
  ungroup() %>% 
  # Calculate increase in SST over the period
  mutate(min_date = as.numeric(as_date(min(data_sst$date))),
         max_date = as.numeric(as_date(max(data_sst$date)))) %>% 
  mutate(sst_increase = ((max_date)*slope+intercept) - ((min_date)*slope+intercept)) %>% 
  select(-min_date, -max_date) %>% 
  # Calculate the warming rate (°C per year)
  mutate(warming_rate = sst_increase/(year(max(data_sst$date))-year(min(data_sst$date)))) %>% 
  filter(area == "Belize")
  
## 4.3 Transform the data ----

data_sst <- data_sst %>% 
  filter(area == "Belize") %>% 
  mutate(date = as_date(date),
         slope = data_warming$slope,
         intercept = data_warming$intercept,
         date_num = as.numeric(date),
         sst_linear = slope*date_num+intercept)

data_sst_point <- data_sst %>% 
  filter(row_number() == 1 | row_number() == nrow(.))

## 4.4 Make the plot ----

ggplot(data = data_sst) +
  geom_line(aes(x = date, y = sst), color = "black", linewidth = 0.25) +
  geom_line(aes(x = date, y = sst_linear), color = "red") +
  geom_point(data = data_sst_point, aes(x = date, y = sst_linear), size = 3, color = "red") +
  annotate(geom = "text", x = as_date("1983-11-01"),
           y = as.numeric(data_sst_point[1,"sst_linear"]), size = 5, hjust = 1,
           label = "y[a]", family = font_choose_graph, color = "red", parse = TRUE) +
  annotate(geom = "text", x = as_date("2025-06-01"),
           y = as.numeric(data_sst_point[2,"sst_linear"]), size = 5, hjust = -0.25,
           label = "y[b]", family = font_choose_graph, color = "red", parse = TRUE) +
  labs(x = "Year", y = "SST (°C)") +
  lims(x = c(as_date("1983-11-01"), as_date("2026-03-01"))) +
  theme_graph()
  
## 4.5 Export the plot ----

ggsave("figs/04_methods/fig-2.png", height = 4.5, width = 5, dpi = fig_resolution)

# 5. Grid for ML predictions ----

## 5.1 Load data ----

data_land <- st_read("data/01_maps/02_clean/05_princeton/land.shp") %>% 
  filter(area == "Cuba")

load("data/09_model-data/data_predictors_pred.RData")

data_predictors_pred <- data_predictors_pred %>% 
  filter(territory == "Cuba") %>% 
  select(decimalLongitude, decimalLatitude) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

load("data/02_misc/data-benthic.RData")

data_benthic <- data_benthic %>% 
  filter(territory == "Cuba") %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

data_reefs <- st_read("data/01_maps/02_clean/02_reefs/reefs.shp") %>% 
  filter(area == "Cuba")

## 5.2 Map of sites with observed data ----

plot_a <- ggplot() +
  geom_sf(data = data_land, fill = "lightgrey") +
  geom_sf(data = data_reefs, col = "#3498db", alpha = 0.8) +
  geom_sf(data = data_benthic, size = 0.75, col = "red", shape = 16) +
  theme_minimal() +
  labs(title = "Sites with observed data") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = font_choose_map)) +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_graph, 
                   text_cex = 0.7, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))

## 5.3 Map of sites on which to make the predictions ----

plot_b <- ggplot() +
  geom_sf(data = data_land, fill = "lightgrey") +
  geom_sf(data = data_predictors_pred, size = 0.75, col = "red", shape = 16) +
  theme_minimal() +
  labs(title = "Sites for making predictions") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = font_choose_map)) +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_graph, 
                   text_cex = 0.7, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))

## 5.4 Combine the plots ----

plot_a + plot_b

## 5.5 Export the plot ----

ggsave("figs/04_methods/fig-3.png", height = 2.5, width = 10, dpi = fig_resolution)
