# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(terra)
library(tidyterra)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/limits_region.R")
source("code/function/theme_map.R")

# 3. Regional map ----

## 3.1 Load and transform data ----

### 3.1.1 Country boundaries ----

data_land <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

### 3.1.2 EEZ ----

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_eez <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_land_cropped <- st_intersection(data_land, data_crop)

data_eez <- st_difference(data_eez, st_union(data_land_cropped))

### 3.1.3 Reefs ----

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")

data_reefs_buffer <- read_sf("data/01_maps/02_clean/02_reefs/reefs_buffer_5.shp")

### 3.1.4 Background RGB tif ----

data_tif <- rast("data/01_maps/01_raw/04_natural-earth/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")

data_tif <- crop(data_tif, data_crop)

data_land <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_boundary_lines_land/ne_10m_admin_0_boundary_lines_land.shp")

data_land_polygons <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_reefs_buffer <- st_difference(data_reefs_buffer, st_union(data_land_polygons))

## 3.2 Make the map ----

plot <- ggplot() +
  geom_spatraster_rgb(data = data_tif, maxcell = 5e+07) +
  geom_sf(data = data_eez, color = "#363737", fill = NA, linewidth = 0.15) +
  geom_sf(data = data_reefs_buffer, color = NA, fill = "#ad5fad", linewidth = 0.15) +
  geom_sf(data = data_land, color = "#363737", fill = NA, linewidth = 0.15) +
  limits_region() +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                   text_cex = 0.6, style = "bar", line_width = 1,  height = unit(0.04, "cm"), line_col = "black",
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black")) +
  theme_map()

ggsave(filename = "figs/01_part-1/fig-1_raw.png", plot = plot,
       width = 7.25, height = 4.75, dpi = fig_resolution)

# 4. Global map ----

## 4.1 Load and transform data ----

### 4.1.1 Load Natural Earth Data ----

data_country <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs = "+proj=eqearth")

data_graticules <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_graticules_20/ne_10m_graticules_20.shp") %>% 
  st_transform(crs = "+proj=eqearth")

### 4.1.2 Change projection of GCRMN regions ----

data_gcrmn_regions <- st_read("data/01_maps/02_clean/01_gcrmn-regions/gcrmn_regions.shp") %>% 
  st_transform(crs = "+proj=eqearth")

### 4.1.3 Create the border of background map ----

lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

background_map_border <- list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(crs = 4326) %>% 
  st_sf() %>%
  st_transform(crs = "+proj=eqearth")

## 4.2 Make the map ----

plot <- ggplot() +
  geom_sf(data = background_map_border, fill = "white", color = "grey30", linewidth = 0.25) +
  geom_sf(data = data_graticules, color = "#ecf0f1", linewidth = 0.25) +
  geom_sf(data = background_map_border, fill = NA, color = "grey30", linewidth = 0.25) +
  geom_sf(data = data_gcrmn_regions, aes(fill = region), show.legend = FALSE) +
  geom_sf(data = data_country, color = "#24252a", fill = "#dadfe1") +
  theme(text = element_text(family = "Open Sans"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(fill = guide_legend(override.aes = list(size = 5, color = NA)))

ggsave(filename = "figs/00_misc/map_regions_raw.png", plot = plot,
       bg = "transparent", height = 5, width = 8, dpi = 300)
