# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(ggspatial)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map_territory.R")

# 3. Export file to complete with parameters for figures ----

generate_file <- FALSE

if(generate_file == TRUE){
  
  read_sf("data/01_maps/02_clean/03_eez/caribbean_eez_sub.shp") %>% 
    filter(!(territory %in% c("Overlapping claim Navassa Island: United States / Haiti / Jamaica",
                              "Overlapping claim: Venezuela / Netherlands (Aruba) / Dominican Republic",
                              "Overlapping claim: Colombia / Dominican Republic / Venezuela",
                              "Overlapping claim: United States (Puerto Rico) / Dominican Republic",
                              "Overlapping claim: Belize / Honduras",
                              "Serrana Bank",
                              "Quitasueño Bank"))) %>% 
    st_drop_geometry() %>% 
    select(territory) %>% 
    arrange(territory) %>% 
    mutate(scale_bar_pos = NA,
           legend_pos_x = NA,
           legend_pos_y = NA,
           fig_width = NA,
           fig_height = NA) %>% 
    write.csv2(., file = "data/02_misc/territories_spatio-temporal_params.csv",
               row.names = FALSE, fileEncoding = "latin1")
  
}

# 4. Load data ----

## 4.1 Country boundaries ----

data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")

## 4.2 EEZ ----

data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_area.shp")

## 4.3 Reefs ----

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")

## 4.4 Reefs buffer ----

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

## 4.5 Select benthic data ----

load("data/02_misc/data-benthic_area.RData")

data_benthic <- data_benthic_area %>% 
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

## 4.6 Layer to mask adjacent territories ----

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

## 4.7 Data with parameters for plots ----

data_params <- read.csv2("data/02_misc/territories_spatio-temporal_params.csv", fileEncoding = "latin1")

## 4.8 Create legend (to avoid absence of legend for territories with no data) ----

data_legend <- data_benthic %>% 
  st_drop_geometry() %>% 
  select(interval_class) %>%
  distinct() %>% 
  mutate(lat = -11.163136, lon = -55.311887) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 5. Create the function to plot territories ----

plot_territories <- function(territory_i){
  
  bbox_i <- st_bbox(data_reefs_buffer %>% filter(territory == territory_i))
  
  data_land_i <- st_intersection(data_land, bbox_i %>% st_as_sfc() %>% st_buffer(1))
  
  if(territory_i == "Florida"){
    
    data_land_i_bis <- data_land %>% 
      filter(territory == "United States")
  
    }else{
    
      data_land_i_bis <- data_land %>% 
        filter(territory == territory_i)
      
    }
  
  data_params_i <- data_params %>% filter(territory == territory_i)
  
  plot <- ggplot() +
    geom_sf(data = data_reefs_buffer %>% filter(territory == territory_i),
            fill = NA, color = "#363737", linewidth = 0.25, linetype = "dashed") +
    geom_sf(data = data_land_i) +
    geom_sf(data = data_crop, fill = "white", color = NA, alpha = 0.6) +
    geom_sf(data = data_land_i_bis) +
    geom_sf(data = data_benthic %>% arrange(interval_class) %>%
              filter(territory == territory_i),
            aes(color = interval_class), show.legend = FALSE) +
    geom_sf(data = data_legend, aes(color = interval_class)) +
    scale_color_manual(values = palette_second,
                       breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                       labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                       drop = FALSE,
                       name = "Number of years\nwith data") +
    guides(color = guide_legend(override.aes = list(size = 3.5))) +
    theme_map_territory() +
    theme(legend.justification.inside = c(as.numeric(data_params_i$legend_pos_x),
                                          as.numeric(data_params_i$legend_pos_y))) +
    coord_sf(xlim = c(bbox_i[1], bbox_i[3]), ylim = c(bbox_i[2], bbox_i[4])) +
    annotation_scale(location = as.character(data_params_i$scale_bar_pos),
                     width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                     text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"), line_col = "black",
                     pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))
  
    ggsave(filename = str_replace_all(paste0("figs/02_part-2/fig-1/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
                                    "---", "-"),
           width = as.numeric(data_params_i$fig_width),
           height = as.numeric(data_params_i$fig_height))
  
}

# 6. Map over the function ----

map(setdiff(unique(data_eez$territory),
            c("Overlapping claim Navassa Island: United States / Haiti / Jamaica",
              "Overlapping claim: Venezuela / Netherlands (Aruba) / Dominican Republic",
              "Overlapping claim: Colombia / Dominican Republic / Venezuela",
              "Overlapping claim: United States (Puerto Rico) / Dominican Republic",
              "Overlapping claim: Belize / Honduras",
              "Serrana Bank",
              "Quitasueño Bank")), # territories for which no chapter will be included
    ~plot_territories(territory_i = .))
