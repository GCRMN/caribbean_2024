plot_region <- function(){
  
  # 1. Load packages ----
  
  library(tidyverse) # Core tidyverse packages
  library(sf)
  library(ggspatial) # For annotation_scale function
  library(terra)
  library(tidyterra)
  
  # 2. Source functions ----
  
  source("code/function/graphical_par.R")
  source("code/function/theme_map.R")
  theme_set(theme_map())
  
  # 3. Load data ----
  
  ## 3.1 Country boundaries ----
  
  data_countries <- read_sf("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
  
  ## 3.2 EEZ ----
  
  data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_eez.shp")
  
  ## 3.3 Land ----
  
  data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")
  
  ## 3.4 Reefs ----
  
  data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")
  
  ## 3.5 Background RGB tif ----
  
  data_tif <- rast("data/01_maps/01_raw/04_natural-earth/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")
  
  data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  data_tif <- crop(data_tif, data_crop)
  
  # 4. Make the basic regional map ----
  
  caribbean_map <- ggplot() +
    geom_spatraster_rgb(data = data_tif, maxcell = 5e+07) +
    geom_sf(data = data_eez, color = "#363737", fill = NA, alpha = 0.1) +
    geom_sf(data = data_land, color = "#363737", fill = NA, alpha = 0.1, linewidth = 0.1) +
    coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))
  
  # 5. Return the result ----
  
  return(caribbean_map)
  
}