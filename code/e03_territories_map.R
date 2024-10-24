# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Load functions ----

source("code/function/graphical_par.R")

# 3. Load data ----

data_land_ne <- read_sf("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_land_ne <- st_intersection(data_land_ne, data_crop)

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")

data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_eez_sub.shp")

data_eez_us <- read_sf("data/01_maps/02_clean/03_eez/caribbean_eez.shp") %>% 
  filter(territory == "United States")

# 4. Create the function ----

plot_territories <- function(territory_i){
  
  data_circle <- tibble(territory = c("Montserrat", "Saint-Barthélemy", "Saba",
                                      "Sint-Eustatius", "Sint-Marteen - Saint-Martin",
                                      "Saint Kitts and Nevis", "Saint Lucia"),
                        lon = c(-62.192897, -62.826132, -63.236996, -62.975556, -63.058154, -62.665062, -60.966089),
                        lat = c(16.731354, 17.897481, 17.631632, 17.487975, 18.063534, 17.270277, 13.903644)) %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326)
  
  if(territory_i %in% c("Montserrat", "Saint-Barthélemy", "Saba", "Sint-Eustatius",
                        "Sint-Marteen - Saint-Martin", "Saint Kitts and Nevis", "Saint Lucia")){

    ggplot() +
      geom_sf(data = data_eez_us, fill = NA, color = "lightgrey", linewidth = 0.05) +
      geom_sf(data = data_eez, fill = NA, color = "lightgrey", linewidth = 0.05) +
      geom_sf(data = data_eez %>% filter(territory == territory_i),
              fill = "#d64541", color = "#d64541", alpha = 0.35, linewidth = 0.05) +
      geom_sf(data = data_circle %>% filter(territory == territory_i) %>% st_buffer(dist = 2),
              fill = "#d64541", color = NA, alpha = 0.2, linewidth = 0.05) +
      geom_sf(data = data_land_ne, linewidth = 0.1) +
      coord_sf(xlim = c(-105, -50), ylim = c(6, 38), expand = FALSE) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5))
    
  }else{
      
    ggplot() +
      geom_sf(data = data_eez_us, fill = NA, color = "lightgrey", linewidth = 0.05) +
      geom_sf(data = data_eez, fill = NA, color = "lightgrey", linewidth = 0.05) +
      geom_sf(data = data_eez %>% filter(territory == territory_i),
              fill = "#d64541", color = "#d64541", alpha = 0.35, linewidth = 0.05) +
      geom_sf(data = data_land_ne, linewidth = 0.1) +
      coord_sf(xlim = c(-105, -50), ylim = c(6, 38), expand = FALSE) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5))
    
  }
  
  ggsave(filename = str_replace_all(paste0("figs/02_part-2/fig-0/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
                                    "---", "-"),
         height = 2.8, width = 4.4, dpi = fig_resolution)
  
}

# 5. Map over the function ----

map(setdiff(unique(data_eez$territory),
            c("Overlapping claim Navassa Island: United States / Haiti / Jamaica",
              "Overlapping claim: Venezuela / Netherlands (Aruba) / Dominican Republic",
              "Overlapping claim: Colombia / Dominican Republic / Venezuela",
              "Overlapping claim: United States (Puerto Rico) / Dominican Republic",
              "Overlapping claim: Belize / Honduras",
              "Serrana Bank",
              "Quitasueño Bank")), # territories for which no chapter will be included
    ~plot_territories(territory_i = .))
