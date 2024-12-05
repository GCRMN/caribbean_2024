# 1. Load packages ----

library(tidyverse)
library(readxl)
library(ggtext)
library(sf)
sf_use_s2(FALSE)
library(ggspatial)
library(terra)
library(tidyterra)
library(ggnewscale)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/data_descriptors.R")
source("code/function/theme_map_area.R")

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

## 4.1 Load data ----

### 4.1.1 Plotting parameters ----

data_params <- read.csv2("data/02_misc/map_areas_params.csv", encoding = "latin1")

### 4.1.2 Labels ----

data_labels <- read.csv2("data/02_misc/map_areas_labels.csv", encoding = "latin1") %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

### 4.1.3 Benthic sites ----

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

### 4.1.4 Topography ----

data_topo <- terra::rast("data/01_maps/02_clean/06_topography/topography.tif")

### 4.1.5 Bathymetry ----

data_bathy <- st_read("data/01_maps/02_clean/07_bathymetry/bathymetry.shp") %>% 
  st_transform(crs = 4326)

### 4.1.6 Land ----

data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")

## 4.2 Create a function to produce the maps ----

plot_map_area <- function(area_i){
  
  # 1. Select parameters for the area
  
  data_params_i <- data_params %>% 
    filter(area == area_i)
  
  data_topo_i <- st_bbox(c(xmin = as.numeric(data_params_i$xmin),
                           xmax = as.numeric(data_params_i$xmax),
                           ymax = as.numeric(data_params_i$ymax),
                           ymin = as.numeric(data_params_i$ymin)),
                         crs = st_crs(4326)) %>% 
    st_as_sfc() %>% 
    st_as_sf() %>% 
    terra::crop(data_topo, .)
  
  # 2. Make the plot
  
  if(area_i == "Flower Garden Banks"){
    
    plot_i <- ggplot() +
      geom_sf(data = data_bathy, aes(fill = zone, color = zone), show.legend = FALSE) +
      scale_fill_gradient2(low = "#dff2f9", mid = "#def6ff", high = "#a7e3fa", midpoint = 4, guide = "none") +
      scale_color_gradient2(low = "#dff2f9", mid = "#def6ff", high = "#a7e3fa", midpoint = 4, guide = "none") +
      ggnewscale::new_scale_fill() +
      ggnewscale::new_scale_color() +
      geom_sf(data = data_land, fill = NA, color = "darkgrey", linewidth = 0.1) +
      geom_sf(data = data_benthic %>% filter(area == area_i) %>% arrange(interval_class) %>% 
                # Add point outside map range to show the legend when no data
                bind_rows(., tibble(interval_class = unique(data_benthic$interval_class),
                                    lat = 0,
                                    long = 0) %>% 
                            st_as_sf(coords = c("long", "lat"), crs = 4326)),
              size = 1.5,
              aes(color = interval_class), show.legend = TRUE) +
      scale_color_manual(values = palette_second,
                         breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE,
                         name = "NUMBER OF YEARS\nWITH DATA") +
      guides(color = guide_legend(override.aes = list(size = 3.5))) +
      geom_sf_text(data = data_labels %>% filter(area == area_i & type == "ocean"),
                   aes(label = label), size = 3,
                   col = "#88b6cd", fontface = "italic", family = font_choose_map) +
      geom_sf_text(data = data_labels %>% filter(area == area_i & type == "area"),
                   aes(label = label), size = 2.25,
                   col = "#2c3e50", fontface = "bold", family = font_choose_map) +
      geom_sf_text(data = data_labels %>% filter(area == area_i & type == "island"),
                   aes(label = label), size = 2, 
                   col = "#6c7a89", family = font_choose_map) +
      annotation_scale(location = as.character(data_params_i$scalebar_pos),
                       width_hint = 0.25, text_family = font_choose_map, text_col = "#2c3e50",
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"), line_col = "#2c3e50",
                       pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"), bar_cols = c("#2c3e50", "#2c3e50")) +
      coord_sf(xlim = c(as.numeric(data_params_i$xmin), as.numeric(data_params_i$xmax)),
               ylim = c(as.numeric(data_params_i$ymin), as.numeric(data_params_i$ymax)),
               expand = FALSE,
               label_axes = list(bottom = "E", top = "E", left = "N")) +
      scale_x_continuous(breaks = eval(parse(text = data_params_i$scale_x))) +
      scale_y_continuous(breaks = eval(parse(text = data_params_i$scale_y))) +
      theme_map_area() +
      labs(x = NULL, y = NULL)
    
  }else{
    
    plot_i <- ggplot() +
      geom_sf(data = data_bathy, aes(fill = zone, color = zone), show.legend = FALSE) +
      scale_fill_gradient2(low = "#dff2f9", mid = "#def6ff", high = "#a7e3fa", midpoint = 4, guide = "none") +
      scale_color_gradient2(low = "#dff2f9", mid = "#def6ff", high = "#a7e3fa", midpoint = 4, guide = "none") +
      ggnewscale::new_scale_fill() +
      ggnewscale::new_scale_color() +
      geom_spatraster(data = data_topo_i, maxcell = 5e+100, show.legend = FALSE) +
      scale_fill_gradientn(values =  c(0, 0.05, 0.1, 0.2, 0.3, 0.6, 1),
                           colours = c("#ecf0f1",  "#eeeeee", "#dadfe1", "#bdc3c7", "darkgrey", "#6c7a89"),
                           na.value = "transparent", guide = "none") +
      ggnewscale::new_scale_fill() +
      geom_sf(data = data_land, fill = NA, color = "darkgrey", linewidth = 0.1) +
      geom_sf(data = data_benthic %>% filter(area == area_i) %>% arrange(interval_class) %>% 
                # Add point outside map range to show the legend when no data
                bind_rows(., tibble(interval_class = unique(data_benthic$interval_class),
                                    lat = 0,
                                    long = 0) %>% 
                            st_as_sf(coords = c("long", "lat"), crs = 4326)),
              size = 1.5,
              aes(color = interval_class), show.legend = TRUE) +
      scale_color_manual(values = palette_second,
                         breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE,
                         name = "NUMBER OF YEARS\nWITH DATA") +
      guides(color = guide_legend(override.aes = list(size = 3.5))) +
      geom_sf_text(data = data_labels %>% filter(area == area_i & type == "ocean"),
                   aes(label = label), size = 3,
                   col = "#88b6cd", fontface = "italic", family = font_choose_map) +
      geom_sf_text(data = data_labels %>% filter(area == area_i & type == "area"),
                   aes(label = label), size = 2.25,
                   col = "#2c3e50", fontface = "bold", family = font_choose_map) +
      geom_sf_text(data = data_labels %>% filter(area == area_i & type == "island"),
                   aes(label = label), size = 2, 
                   col = "#6c7a89", family = font_choose_map) +
      annotation_scale(location = as.character(data_params_i$scalebar_pos),
                       width_hint = 0.25, text_family = font_choose_map, text_col = "#2c3e50",
                       text_cex = 0.7, style = "bar", line_width = 1, height = unit(0.04, "cm"), line_col = "#2c3e50",
                       pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"), bar_cols = c("#2c3e50", "#2c3e50")) +
      coord_sf(xlim = c(as.numeric(data_params_i$xmin), as.numeric(data_params_i$xmax)),
               ylim = c(as.numeric(data_params_i$ymin), as.numeric(data_params_i$ymax)),
               expand = FALSE,
               label_axes = list(bottom = "E", top = "E", left = "N")) +
      scale_x_continuous(breaks = eval(parse(text = data_params_i$scale_x))) +
      scale_y_continuous(breaks = eval(parse(text = data_params_i$scale_y))) +
      theme_map_area() +
      labs(x = NULL, y = NULL)
    
  }
  
  # 3. Export the plot
  
  ggsave(filename = str_replace_all(paste0("figs/02_part-2/fig-1/", str_replace_all(str_to_lower(area_i), " ", "-"), ".png"),
                                    "---", "-"),
         plot = plot_i,
         width = as.numeric(data_params_i$export_width),
         height = as.numeric(data_params_i$export_height))
  
}

## 4.3 Map over the function ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp") %>% 
  st_drop_geometry() %>% 
  filter(!(area %in% c("Navassa Island", "Guatemala"))) %>% 
  distinct() %>% 
  arrange(area)

map(data_area, ~plot_map_area(area_i = .))





#### TEST LABELS ##################

data_labels <- tibble(area = "Guadeloupe",
                      label = c("Atlantic\nOcean", "Caribbean\nSea", "GUADELOUPE", "MONTSERRAT", "DOMINICA",
                                "La Désirade"),
                      type = c("ocean", "ocean", "area", "area", "area", "island"),
                      lat = c(16.6, 15.8, 16.1, 16.7, 15.68, 16.35),
                      long = c(-60.6, -62, -60.85, -61.975, -61.45, -60.85)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)
