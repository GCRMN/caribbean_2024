# 1. Load packages ----

library(tidyverse)
library(readxl)
library(ggtext)
library(sf)
sf_use_s2(FALSE)
library(ggspatial)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/data_descriptors.R")
source("code/function/theme_map_territory.R")

# 3. Plot of number of sites per year and per area ----

## 3.1 Select benthic data ----

load("data/02_misc/data-benthic.RData")

## 3.2 Transform data ----

data_sources <- read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  select(datasetID, rightsHolder) %>% 
  distinct()

data_year_dataset <- data_benthic %>% 
  group_by(datasetID, area, year) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  select(datasetID, area, year, nb_sites) %>% 
  complete(nesting(datasetID, area),
           year = 1980:2024,
           fill = list(nb_sites = 0)) %>% 
  left_join(., data_sources) %>% 
  mutate(label = paste0("<b>", datasetID,
                        "</b><br><span style = 'font-size:10pt'>(",
                        rightsHolder, ")</span>"))

## 3.3 Create a function to produce the plot ----

plot_year_dataset <- function(area_i){
  
  data_year_dataset_i <- data_year_dataset %>% 
    filter(area == area_i)
  
  nb_datasets_i <- length(unique(data_year_dataset_i$datasetID))
  
  plot_i <- ggplot(data = data_year_dataset_i,
                   aes(x = year, y = label, fill = nb_sites)) +
    geom_tile(color = "white", height = 0.6, linewidth = 0.6) +
    theme_graph() +
    labs(y = NULL, x = "Year") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2025)) +
    theme(legend.title.position = "top",
          legend.title = element_text(size = 10, hjust = 1, face = "bold", color = "#2c3e50"),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.justification = "right",
          axis.text.y = element_markdown())
  
  if(max(data_year_dataset_i$nb_sites) == 1){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, 2),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else if(max(data_year_dataset_i$nb_sites) == 2){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else if(max(data_year_dataset_i$nb_sites) == 3){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2, 3),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[4]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, 1, 2, 3)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else{
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0)),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[3],
                                   palette_second[4], palette_second[5]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0))),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }
  
  ggsave(filename = paste0("figs/02_part-2/fig-4/",
                           str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = (2 + (3*0.3*nb_datasets_i)), width = 9, dpi = fig_resolution)
  
}

## 3.4 Map over the function ----

map(unique(data_benthic$area), ~plot_year_dataset(area_i = .))













# 4. Map of areas ----

## 4.1 Export file to complete with parameters for figures ----

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

## 4.2 Load data ----

### 4.2.1 Country boundaries ----

data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")

### 4.2.2 EEZ ----

data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_area.shp")

### 4.2.3 Reefs ----

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")

### 4.2.4 Reefs buffer ----

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

### 4.2.5 Select benthic data ----

load("data/02_misc/data-benthic.RData")

data_benthic <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, area) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, area) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

### 4.2.6 Layer to mask adjacent territories ----

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

### 4.2.7 Data with parameters for plots ----

data_params <- read.csv2("data/02_misc/territories_spatio-temporal_params.csv", fileEncoding = "latin1")

### 4.2.8 Create legend (to avoid absence of legend for territories with no data) ----

data_legend <- data_benthic %>% 
  st_drop_geometry() %>% 
  select(interval_class) %>%
  distinct() %>% 
  mutate(lat = -11.163136, lon = -55.311887) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

## 4.3 Create the function to plot territories ----

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

## 4.4 Map over the function ----

map(setdiff(unique(data_eez$territory),
            c("Overlapping claim Navassa Island: United States / Haiti / Jamaica",
              "Overlapping claim: Venezuela / Netherlands (Aruba) / Dominican Republic",
              "Overlapping claim: Colombia / Dominican Republic / Venezuela",
              "Overlapping claim: United States (Puerto Rico) / Dominican Republic",
              "Overlapping claim: Belize / Honduras",
              "Serrana Bank",
              "Quitasueño Bank")), # territories for which no chapter will be included
    ~plot_territories(territory_i = .))
