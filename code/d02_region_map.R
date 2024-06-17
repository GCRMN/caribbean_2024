# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(patchwork)
library(ggspatial) # For annotation_scale function

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")

# 3. Load data ----

## 3.1 Country boundaries ----

data_countries <- read_sf("data/01_shp/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

## 3.2 Bathymetry ----

load("data/01_shp/02_clean/04_natural-earth/ne_10m_bathymetry_all.RData")

## 3.3 EEZ ----

data_eez <- read_sf("data/01_shp/02_clean/03_eez/caribbean_eez.shp")

## 3.4 Reefs ----

data_reefs <- read_sf("data/01_shp/02_clean/02_reefs/reefs.shp")

## 3.5 Create the tropics ----

data_tropics <- tibble(long = c(-180, 180, -180, 180, -180, 180), 
                       lat = c(0, 0, 23.43656, 23.43656, -23.43656, -23.43656), 
                       tropic = c("Equator", "Equator", "Tropic of Cancer", "Tropic of Cancer",
                                  "Tropic of Capricorne", "Tropic of Capricorne")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(tropic) %>% 
  summarise() %>% 
  st_cast("LINESTRING")

# 3.6 Bbox for alpha on bathymetry ---

data_alpha <- tibble(lat = c(0, 50),
                     lon = c(-130, -40)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_alpha <- data_eez %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_difference(data_alpha, .)

# 4. Create text annotation ----

data_labels_tropic <- tibble(long = c(-57.5),
                      lat = c(24.5),
                      text = c("Tropic of Cancer")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

data_labels_atlantic <- tibble(long = c(-60),
                      lat = c(27.5),
                      text = c("Atlantic Ocean")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

data_labels_land <- tibble(long = c(-95, -99.5, -66, -75),
                      lat = c(33.5, 20, 8, 7.5),
                      text = c("United States", "Mex.", "Venezuela", "Col.")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# 5. Make the basic regional map ----

caribbean_map <- ggplot() +
  geom_sf(data = data_bathy %>% filter(depth == 0), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 200), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 1000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 2000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 3000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 4000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 5000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 6000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 7000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 8000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 9000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 10000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  scale_fill_identity() +
  #geom_sf(data = data_alpha, fill = "white", alpha = 0.6) +
  # Tropics
  geom_sf(data = data_tropics, linetype = "dashed", color = "white", linewidth = 0.25) +
  # EEZ
  geom_sf(data = data_eez, color = "#363737", fill = "#e4e9ed", alpha = 0.1) +
  # Countries
  geom_sf(data = data_countries, fill = "grey", col = "darkgrey") +
  # Text annotations
  geom_sf_text(data = data_labels_tropic, aes(label = text), color = "white",
               fontface = "italic", size = 3.5, family = font_choose_map) +
  geom_sf_text(data = data_labels_atlantic, aes(label = text), color = "#1e517b",
               fontface = "italic", size = 3.5, family = font_choose_map) +
  geom_sf_text(data = data_labels_land, aes(label = text), color = "#363737",
               fontface = "italic", size = 3.5, family = font_choose_map) +
  theme_map()

save(caribbean_map, file = "data/02_misc/caribbean_map.RData")

# 6. Add annotations ----

caribbean_map +
  # Reefs
  geom_sf(data = data_reefs, color = palette_second[4]) +
  # Parameters
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, 
                   text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black")) +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

# 7. Export the map ----

ggsave(filename = "figs/01_part-1/fig-1.png", width = 7.5, height = 4.75, dpi = fig_resolution)
