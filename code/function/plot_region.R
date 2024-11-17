plot_region <- function(scale = TRUE){
  
  # 1. Load packages ----
  
  library(tidyverse) # Core tidyverse packages
  library(sf)
  sf_use_s2(FALSE)
  library(ggspatial) # For annotation_scale function
  library(terra)
  library(tidyterra)
  
  # 2. Source functions ----
  
  source("code/function/graphical_par.R")
  source("code/function/theme_map.R")
  theme_set(theme_map())
  
  # 3. Load data ----
  
  ## 3.1 Country boundaries ----
  
  data_land <- st_read("data/01_maps/01_raw//04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
  
  ## 3.2 Reefs ----
  
  data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")
  
  ## 3.3 Background RGB tif ----
  
  data_tif <- rast("data/01_maps/01_raw/04_natural-earth/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")
  
  data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  data_tif <- crop(data_tif, data_crop)
  
  ## 3.4 EEZ ----
  
  data_eez <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

  data_land_cropped <- st_intersection(data_land, data_crop)
  
  data_eez <- st_difference(data_eez, st_union(data_land_cropped))

  data_land <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_boundary_lines_land/ne_10m_admin_0_boundary_lines_land.shp")

  # 4. Make the basic regional map ----
  
  caribbean_map <- ggplot() +
    geom_spatraster_rgb(data = data_tif, maxcell = 5e+07) +
    geom_sf(data = data_eez, color = "#363737", fill = NA, linewidth = 0.15) +
    geom_sf(data = data_land, color = "#363737", fill = NA, linewidth = 0.15) +
    coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))
  
  if(scale == TRUE){
    
    caribbean_map <- caribbean_map +
      annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                       text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"), line_col = "black",
                       pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))
    
  }
  
  # 5. Return the result ----
  
  return(caribbean_map)
  
}
