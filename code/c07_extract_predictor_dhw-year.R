# 1. Required packages ----

library(tidyverse)
library(terra)
library(sf)
sf_use_s2(FALSE)

# 2. Load site coordinates data ----

site_coords <- st_read("data/04_site-coords/site-coords_all.shp") %>% 
  arrange(type, site_id)

# 3. List of ncdf files ----

ncdf_files <- list.files("data/08_dhw-year/", full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(year = as.numeric(str_sub(value, -8, -5)))

# 4. Create a function to extract max yearly DHW value for each site ----

extract_dhw <- function(year_i){
  
  # 1. Filter year i
  
  ncdf_files_i <- ncdf_files %>% 
    filter(year == year_i)
  
  # 2. Load the raster
  
  ncdf_i <- terra::rast(ncdf_files_i$value)

  crs(ncdf_i) <- "epsg:4326"
  
  # 3. Extract DHW values
  
  results <- terra::extract(x = ncdf_i, y = site_coords, method = "bilinear") %>% 
    as_tibble() %>% 
    mutate(year = year_i) %>% 
    bind_cols(site_coords %>% st_drop_geometry(), .) %>% 
    select(-ID) %>% 
    rename(pred_dhw_max = max)
  
  return(results)
  
}

# 5. Map over the function ----

pred_dhw_max <- map_dfr(ncdf_files$year, ~extract_dhw(year_i = .))

# 6. Export the results ----

write.csv(pred_dhw_max, file = "data/10_predictors/pred_dhw_max.csv", row.names = FALSE)
