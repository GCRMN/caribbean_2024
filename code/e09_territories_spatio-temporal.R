# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(ggspatial)
library(terra)
library(tidyterra)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map_territory.R")

# 3. Load data ----

## 3.1 Country boundaries ----

data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")

## 3.2 EEZ ----

data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_eez_sub.shp")

## 3.3 Reefs ----

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")

## 3.4 Reefs buffer ----

data_reefs_buffer <- st_read("data/01_maps/02_clean/02_reefs/reefs_buffer_100.shp") %>% 
  # Correct the issue of encoding GEE export
  mutate(territory = str_replace_all(territory, c("Cura\\?ao" = "Curaçao",
                                                  "Quitasue\\?o Bank" = "Quitasueño Bank",
                                                  "Saint-Barth\\?lemy" = "Saint-Barthélemy"))) %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  group_by(territory) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()

## 3.5 Select benthic data ----

load("data/02_misc/data-benthic.RData")

data_benthic <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, territory) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, territory) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

## 3.6 Layer to mask adjacent territories ----

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

# 4. Create the function to plot territories

plot_territories <- function(territory_i, legend_x, legend_y){
  
  bbox_i <- st_bbox(data_reefs_buffer %>% filter(territory == territory_i))
  
  data_land_i <- st_intersection(data_land, bbox_i %>% st_as_sfc() %>% st_buffer(1))
  
  if(territory_i == "Florida"){
    
    data_land_i_bis <- data_land %>% 
      filter(territory == "United States")
  
    }else{
    
      data_land_i_bis <- data_land %>% 
        filter(territory == territory_i)
      
  }
  
  plot <- ggplot() +
    geom_sf(data = data_reefs_buffer %>% filter(territory == territory_i),
            fill = NA, color = "#363737", linewidth = 0.25, linetype = "dashed") +
    geom_sf(data = data_land_i) +
    geom_sf(data = data_crop, fill = "white", color = NA, alpha = 0.6) +
    geom_sf(data = data_land_i_bis) +
    geom_sf(data = data_benthic %>% arrange(interval_class) %>%
              filter(territory == territory_i),
            aes(color = interval_class)) +
    scale_color_manual(values = palette_second,
                       breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                       labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                       drop = FALSE,
                       name = "Number of years\nwith data") +
    guides(color = guide_legend(override.aes = list(size = 3.5))) +
    theme_map_territory() +
    theme(legend.position.inside = c(legend_x, legend_y)) +
    coord_sf(xlim = c(bbox_i[1], bbox_i[3]), ylim = c(bbox_i[2], bbox_i[4])) +
    annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                     text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"), line_col = "black",
                     pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))
  
    ggsave(filename = str_replace_all(paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
                                    "---", "-"))
  
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
    ~plot_territories(territory_i = ., legend_x = 0.8, legend_y = 0.8))
