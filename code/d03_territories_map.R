# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(s2)
library(sf)
sf_use_s2(TRUE)
library(ggspatial) # For annotation_scale function

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")
theme_set(theme_map())

# 3. Load data ----

## 3.1 Country boundaries ----

data_land <- read_sf("data/01_shp/02_clean/05_princeton/land.shp")

## 3.2 EEZ ----

data_eez <- read_sf("data/01_shp/02_clean/03_eez/caribbean_eez.shp")

## 3.3 Reefs ----

data_reefs <- read_sf("data/01_shp/02_clean/02_reefs/reefs.shp")

# 4. Create the function to plot territories

plot_territories <- function(territory_i){
  
  bbox_i <- st_bbox(data_land %>% filter(NAME_ISO == str_to_upper(territory_i)))
  
  ggplot() +
    geom_sf(data = data_land) +
    geom_sf(data = data_eez %>% filter(TERRITORY1 == str_to_sentence(territory_i)), fill = NA, color = "white") +
    coord_sf(xlim = c(bbox_i[1], bbox_i[3]), ylim = c(bbox_i[2], bbox_i[4]))
  
  ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"))
  
}

# 5. Map over the function ----

map(unique(data_land$NAME_ISO), ~plot_territories(territory_i = .))
