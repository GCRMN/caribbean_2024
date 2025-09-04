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

data_sources <- read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  select(datasetID, rightsHolder) %>% 
  distinct()

data_year_dataset <- data_benthic %>% 
  filter(!(area %in% c("Navassa Island"))) %>% 
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
          legend.title = element_text(size = 10, hjust = 1, color = "#2c3e50"),
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

map(unique(data_year_dataset$area), ~plot_year_dataset(area_i = .))

# 4. Map of areas ----

## 4.1 Load data ----

### 4.1.1 Plotting parameters ----

data_params <- read.csv2("data/02_misc/map_areas_params.csv", encoding = "latin1")

### 4.1.2 Benthic sites ----

data_benthic <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, area) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, area) %>% 
  count(name = "nb_years") %>% 
  ungroup() %>% 
  mutate(int_class = cut(nb_years, 
                         breaks = c(-Inf, 1, 5, 10, 15, Inf),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         int_class = as.factor(int_class)) %>% 
  arrange(int_class) %>% 
  select(-nb_years) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

### 4.1.3 Land ----

data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")

## 4.2 Create a function to produce the maps ----

plot_map_area <- function(area_i){
  
  # 1. Select parameters for the area
  
  data_params_i <- data_params %>% 
    filter(area == area_i)

  # 2. Make the plot
  
  if(area_i == "Flower Garden Banks"){
    
    plot_i <- ggplot() +
      geom_sf(data = data_land, fill = "#dadfe1", color = "darkgrey", linewidth = 0.1) +
      geom_sf(data = data_benthic %>% filter(area == area_i) %>% arrange(int_class) %>% 
                # Add point outside map range to show the legend when no data
                bind_rows(., tibble(int_class = unique(data_benthic$int_class),
                                    lat = 0,
                                    long = 0) %>% 
                            st_as_sf(coords = c("long", "lat"), crs = 4326)),
              size = 1.5,
              aes(color = int_class), show.legend = TRUE) +
      scale_color_manual(values = palette_second,
                         breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE,
                         name = "NUMBER OF YEARS\nWITH DATA") +
      guides(color = guide_legend(override.aes = list(size = 3.5))) +
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
    
  }else if(area_i == "United States Virgin Islands"){
    
    # Create polygons for MPA
    
    data_mpa <- tibble(point = c("A", "B", "C", "D"),
                       latitude = c(18.1983, 18.19408, 18.1843, 18.18852),
                       longitude = c(-64.9388, -64.93708, -64.9635, -64.96522))
    
    coords <- data_mpa %>%
      select(longitude, latitude) %>%
      as.matrix() %>%
      rbind(c(data_mpa$longitude[1], data_mpa$latitude[1]))
    
    data_mpa_1 <- st_sf(mpa = "Grammanik Bank",
                        geometry = st_sfc(st_polygon(list(coords)), crs = 4326))
    
    data_mpa <- tibble(point = c("A", "B", "C", "D"),
                       latitude = c(18.22, 18.22, 18.19667, 18.17833),
                       longitude = c(-65.1, -64.98333, -64.98333, -65.1))
    
    coords <- data_mpa %>%
      select(longitude, latitude) %>%
      as.matrix() %>%
      rbind(c(data_mpa$longitude[1], data_mpa$latitude[1]))
    
    data_mpa_2 <- st_sf(mpa = "Hind Bank",
                        geometry = st_sfc(st_polygon(list(coords)), crs = 4326))
    
    data_mpa <- bind_rows(data_mpa_1, data_mpa_2)
    
    plot_i <- ggplot() +
      geom_sf(data = data_land, fill = "#dadfe1", color = "darkgrey", linewidth = 0.1) +
      geom_sf(data = data_benthic %>% filter(area == area_i) %>% arrange(int_class) %>% 
                # Add point outside map range to show the legend when no data
                bind_rows(., tibble(int_class = unique(data_benthic$int_class),
                                    lat = 0,
                                    long = 0) %>% 
                            st_as_sf(coords = c("long", "lat"), crs = 4326)),
              size = 1.5,
              aes(color = int_class), show.legend = TRUE) +
      scale_color_manual(values = palette_second,
                         breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE,
                         name = "NUMBER OF YEARS\nWITH DATA") +
      geom_sf(data = data_mpa, color = "black", fill = "transparent", linetype = "dashed") +
      guides(color = guide_legend(override.aes = list(size = 3.5))) +
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
    
    rm(data_mpa, data_mpa_1, data_mpa_2, coords)
   
  }else{
    
    plot_i <- ggplot() +
      geom_sf(data = data_land, fill = "#dadfe1", color = "darkgrey", linewidth = 0.1) +
      geom_sf(data = data_benthic %>% filter(area == area_i) %>% arrange(int_class) %>% 
                # Add point outside map range to show the legend when no data
                bind_rows(., tibble(int_class = unique(data_benthic$int_class),
                                    lat = 0,
                                    long = 0) %>% 
                            st_as_sf(coords = c("long", "lat"), crs = 4326)),
              size = 1.5,
              aes(color = int_class), show.legend = TRUE) +
      scale_color_manual(values = palette_second,
                         breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE,
                         name = "NUMBER OF YEARS\nWITH DATA") +
      guides(color = guide_legend(override.aes = list(size = 3.5))) +
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
  
  ggsave(filename = str_replace_all(paste0("figs/02_part-2/fig-1/",
                                           str_replace_all(str_to_lower(area_i), " ", "-"), "_raw.png"),
                                    "---", "-"),
         plot = plot_i,
         width = as.numeric(data_params_i$export_width),
         height = as.numeric(data_params_i$export_height))
  
}

## 4.3 Map over the function ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp") %>% 
  st_drop_geometry() %>% 
  filter(!(area %in% c("Navassa Island"))) %>% 
  distinct() %>% 
  arrange(area) %>% 
  pull()

map(data_area, ~plot_map_area(area_i = .))
