# 1. Required packages and functions ----

library(tidyverse)
library(sf)
sf_use_s2(TRUE)

# 2. Load data ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_eez_land <- st_read("data/01_maps/01_raw/02_eez/EEZ_land_union_v4_202410.shp") %>% 
  rename("area" = "TERRITORY1") %>% 
  filter(area %in% c("Belize", "Colombia", "Costa Rica", "Dominican Republic",
                     "Florida", "Guatemala", "Haiti", "Honduras", "Mexico",
                     "Nicaragua", "Panama", "Venezuela", "United States",
                     "Sint-Maarten", "Collectivity of Saint Martin")) %>% 
  filter(POL_TYPE == "Union EEZ and country") %>% 
  select(area) %>% 
  mutate(area = str_replace_all(area, c("United States" = "Florida",
                                        "Collectivity of Saint Martin" = "Saint-Martin")))

data_buffer <- st_read("data/01_maps/02_clean/02_reefs/reefs_buffer_20.shp") %>% 
  mutate(area = case_when(area == "Cura?ao" ~ "Curaçao",
                          area == "Saint-Barth?lemy" ~ "Saint-Barthélemy",
                          TRUE ~ area))

# 3. Create the function ----

difference_buffer <- function(area_i, plot = FALSE){
  
  if(!(area_i %in% c("Belize", "Colombia", "Costa Rica", "Dominican Republic",
                     "Florida", "Guatemala", "Haiti", "Honduras", "Mexico (Caribbean Sea)",
                     "Mexico (Gulf of Mexico)", "Nicaragua", "Panama", "Venezuela", "Saint-Martin", "Sint-Maarten"))){
    
    data_buffer_i <- data_buffer %>% filter(area == area_i)
    
    data_area_i <- data_area %>% filter(area == area_i)
    
    data_result <- st_intersection(data_buffer_i, data_area_i)
    
  }else if(area_i %in% c("Belize", "Colombia", "Costa Rica", "Dominican Republic",
                         "Guatemala", "Haiti", "Honduras", "Nicaragua", "Panama", "Venezuela",
                         "Florida", "Saint-Martin", "Sint-Maarten")){
    
    data_buffer_i <- data_buffer %>% filter(area == area_i)
    
    data_area_i <- data_eez_land %>% filter(area == area_i)
    
    data_result <- st_intersection(data_buffer_i, data_area_i)
    
  }else if(area_i %in% c("Mexico (Caribbean Sea)", "Mexico (Gulf of Mexico)")){
    
    data_buffer_i <- data_buffer %>% filter(area == area_i)
    
    data_area_i <- data_eez_land %>% filter(area == "Mexico") %>% st_make_valid()

    data_result <- st_intersection(data_buffer_i, data_area_i)
    
  }else{
    
    stop(paste0(area_i), " is not a recognised area")
    
  }
  
  if(plot == TRUE){
    
    plot_i <- ggplot() +
      geom_sf(data = data_area_i) +
      geom_sf(data = data_buffer_i, fill = "red") + 
      geom_sf(data = data_result, fill = "lightgreen") +
      labs(title = area_i)
    
    plot(plot_i)
    
  }
  
  return(data_result %>% select(-area.1))
  
}

# 4. Map over the function ----

data_buffer_area <- map_dfr(sort(unique(data_area$area)),
                            ~difference_buffer(area_i = ., plot = TRUE))

# 5. Export the data ----

st_write(data_buffer_area, "data/01_maps/02_clean/02_reefs/reefs_buffer_area.shp", append = FALSE)

# 6. Export EEZ land data ----

## 6.1 Combine area and EEZ land ----

data_area <- data_area %>% 
  filter(!(area %in% c("Belize", "Colombia", "Costa Rica", "Dominican Republic",
                       "Florida", "Guatemala", "Haiti", "Honduras", "Mexico (Caribbean Sea)",
                       "Mexico (Gulf of Mexico)", "Nicaragua", "Panama", "Venezuela", "Flower Garden Banks",
                       "Sint-Maarten", "Saint-Martin")))

data_eez_land <- st_read("data/01_maps/01_raw/02_eez/EEZ_land_union_v4_202410.shp") %>% 
  filter(POL_TYPE == "Union EEZ and country" & TERRITORY1 %in% c("Belize", "Colombia", "Costa Rica", "Dominican Republic",
                                                                 "United States", "Guatemala", "Haiti", "Honduras", "Mexico",
                                                                 "Nicaragua", "Panama", "Venezuela",                                                                #
                                                                 "Sint-Maarten", "Collectivity of Saint Martin")) %>% 
  select(TERRITORY1) %>% 
  rename(area = TERRITORY1) %>%
  mutate(area = str_replace_all(area, c("Collectivity of Saint Martin" = "Saint-Martin"))) %>% 
  bind_rows(., data_area) %>% 
  st_make_valid() %>% 
  group_by(area) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("MULTIPOLYGON") %>%
  ungroup() %>% 
  arrange(area)
  
ggplot(data = data_eez_land) +
  geom_sf(alpha = 0.5)

data_eez_land_seaflower <- data_eez_land %>% 
  filter(area == "Seaflower Biosphere Reserve")

sf_use_s2(FALSE)

data_eez_land <- data_eez_land %>% 
  filter(area != "Seaflower Biosphere Reserve") %>% 
  st_difference(., data_eez_land_seaflower) %>% 
  bind_rows(., data_eez_land_seaflower) %>% 
  arrange(area) %>%
  select(-area.1)

ggplot(data = data_eez_land) +
  geom_sf(alpha = 0.5)

## 6.2 Export the data ----

st_write(data_eez_land, "data/01_maps/02_clean/03_eez/caribbean_area_eez.shp", append = FALSE)
