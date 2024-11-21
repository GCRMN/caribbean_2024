# 1. Required packages and functions ----

library(tidyverse)
library(sf)
sf_use_s2(TRUE)

# 2. Load data ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_buffer <- st_read("data/01_maps/02_clean/02_reefs/reefs_buffer_20.shp")



data_eez_land <- st_read("data/01_maps/01_raw/02_eez/EEZ_land_union_v4_202410.shp") %>% 
  filter(TERRITORY1 %in% c(unique(data_area$area), "Collectivity of Saint Martin", "Sint-Maarten")) %>% 
  select(TERRITORY1) %>% 
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Collectivity of Saint Martin" = "Sint-Maarten - Saint-Martin",
                                                    "Sint-Maarten" = "Sint-Maarten - Saint-Martin")))



difference_buffer <- function(area_i, plot = FALSE){
  
  data_mask <- data_eez_land %>% 
    filter(TERRITORY1 != area_i) %>% 
    summarise(geometry = st_union(geometry))
  
  data_buffer_i <- data_buffer %>% 
    filter(area == area_i) %>% 
    st_difference(., data_mask)
  
  if(plot == TRUE){
    
    plot_i <- ggplot() +
      geom_sf(data = data_area %>% filter(area == area_i), alpha = 0.5) +
      geom_sf(data = data_buffer %>% filter(area == area_i), alpha = 0.5) +
      geom_sf(data = data_buffer_i, fill = "red", alpha = 0.5)
    
    plot(plot_i)
    
  }
  
  return(data_buffer_i)
  
}

difference_buffer(area_i = "Sint-Maarten - Saint-Martin", plot = TRUE)


"Flower Garden Banks"         "Florida"                     "Sint-Maarten - Saint-Martin" "Mexico (Gulf of Mexico)"    
 "Mexico (Caribbean Sea)"      "Seaflower Biosphere Reserve"

# Export data for GEE for the join population
