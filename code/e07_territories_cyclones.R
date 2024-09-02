# 1. Required packages ----

library(tidyverse)
library(ggrepel)
library(glue)
library(sf)
sf_use_s2(FALSE)
library(ggsflabel)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map_territory.R")

# 3. Plots of cyclone maximum wind speed over time ----

## 3.1 Transform data ----

load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  filter(!(territory %in% c("Quitasueño Bank", "Serrana Bank", "Navassa Island"))) %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1) %>% 
  mutate(ts_name = str_to_sentence(ts_name),
         max_saffir = as.factor(max_saffir)) %>% 
  # Add cyclone position by wind_speed
  arrange(territory, desc(windspeed)) %>% 
  group_by(territory) %>% 
  mutate(position = row_number())

## 3.2 Create the function ----

map_cyclone_plot <- function(territory_i){
  
  # 1. Filter
  
  data_cyclones_i <- data_cyclones %>% 
    filter(territory == territory_i)
  
  # 2. Make the plot 
  
  plot_i <- ggplot(data = data_cyclones_i, aes(x = time, y = windspeed)) +
    geom_point(aes(fill = max_saffir), color = "white", shape = 21, size = 4.5,
               show.legend = c(shape = TRUE)) +
    geom_label_repel(data = data_cyclones_i %>% filter(position %in% 1:15), # Label only the first 15 cyclones
                     aes(label = ts_name, color = max_saffir), fill = "white", alpha = 0.9,
                     label.r = unit(0.4, "lines"), show.legend = FALSE,
                     max.overlaps = getOption("ggrepel.max.overlaps", default = 15)) +
    scale_y_continuous(breaks = c(0, 50 ,100, 150, 200, 250, 300), limits = c(0, 300)) +
    scale_x_date(limits = c(ymd("1980-01-01"), ymd("2025-01-01"))) +
    coord_cartesian(ylim = c(14.25, 310)) +
    scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                      labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                      values = c(palette_second[2:5], "black"),
                      name = "Saffir-Simpson\ncategory",
                      drop = FALSE) +
    scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                       labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                       values = c(palette_second[2:5], "black"),
                       name = "Saffir-Simpson\ncategory",
                       drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(size = 4))) +
    labs(x = "Year", y = bquote("Wind speed (km."~h^-1*")")) +
    theme_graph() +
    theme(text = element_text(size = 13),
          legend.position = "right",
          legend.direction = "vertical",
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  # 3. Save the plot
  
  ggsave(filename = paste0("figs/02_part-2/fig-4/",
                           str_replace_all(str_replace_all(str_to_lower(territory_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = 3.5, width = 9, dpi = fig_resolution)
  
}

## 3.3 Map over the function ----

map(setdiff(unique(data_cyclones$territory),
            c("Overlapping claim Navassa Island: United States / Haiti / Jamaica",
              "Overlapping claim: Venezuela / Netherlands (Aruba) / Dominican Republic",
              "Overlapping claim: Colombia / Dominican Republic / Venezuela",
              "Overlapping claim: United States (Puerto Rico) / Dominican Republic",
              "Overlapping claim: Belize / Honduras",
              "Serrana Bank",
              "Quitasueño Bank")), # territories for which no chapter will be included
    ~map_cyclone_plot(territory_i = .))

# 4. Maps of cyclone trajectories ----

## 4.1 Load data ----

### 4.1.1 Country boundaries ----

data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")

### 4.1.2 EEZ ----

data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_eez_sub.shp")

### 4.1.3 Reefs ----

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")

### 4.1.4 Reefs buffer ----

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

### 4.1.5 Cyclones ----

load("data/07_cyclones/02_cyclones_extracted.RData")
load("data/07_cyclones/01_cyclones_lines.RData")
load("data/07_cyclones/01_cyclones_points.RData")

data_cyclones <- data_cyclones %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1) %>% 
  mutate(time_range = case_when(time > as_date("1980-01-01") & time <= as_date("1987-12-31") ~ "1980 - 1987",
                                time > as_date("1988-01-01") & time <= as_date("1995-12-31") ~ "1988 - 1995",
                                time > as_date("1996-01-01") & time <= as_date("2003-12-31") ~ "1996 - 2003",
                                time > as_date("2004-01-01") & time <= as_date("2011-12-31") ~ "2004 - 2011",
                                time > as_date("2012-01-01") & time <= as_date("2019-12-31") ~ "2012 - 2019",
                                time > as_date("2020-01-01") & time <= as_date("2024-12-31") ~ "2020 - 2024"),
         time_range = as.factor(time_range),
         time_range = fct_expand(time_range, "1980 - 1987", "1988 - 1995", "1996 - 2003", "2004 - 2011",
                                 "2012 - 2019", "2020 - 2024"),
         time_range = fct_relevel(time_range, c("1980 - 1987", "1988 - 1995", "1996 - 2003", "2004 - 2011",
                                                "2012 - 2019", "2020 - 2024")),
         ts_name = str_to_sentence(ts_name),
         max_saffir = as.factor(max_saffir)) %>% 
  # Add cyclone position by wind_speed
  arrange(territory, desc(windspeed)) %>% 
  group_by(territory) %>% 
  mutate(position = row_number()) %>% 
  ungroup()

### 4.1.6 Layer to mask adjacent territories ----

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

## 4.2 Create the function to plot territories ----

plot_territories <- function(territory_i){
  
  bbox_i <- st_bbox(data_reefs_buffer %>% filter(territory == territory_i))
  
  data_land_i <- st_intersection(data_land, bbox_i %>% st_as_sfc() %>% st_buffer(1))
  
  data_eez_i <- data_eez %>% filter(territory == territory_i)
  
  if(territory_i == "Florida"){
    
    data_land_i_bis <- data_land %>% 
      filter(territory == "United States")
    
  }else{
    
    data_land_i_bis <- data_land %>% 
      filter(territory == territory_i)
    
  }
  
  data_cyclones_i <- data_cyclones %>% 
    filter(territory == territory_i)
  
  data_ts_lines_i <- left_join(data_cyclones_i, data_ts_lines) %>% 
    st_as_sf()
  
  data_ts_points_i <- data_ts_points %>% 
    filter(ts_id %in% unique(data_cyclones_i$ts_id)) %>% 
    mutate(ts_name = str_to_sentence(ts_name)) %>% 
    left_join(., data_cyclones_i %>% select(ts_id, ts_name, time_range, position)) %>% 
    mutate(saffir = case_when(windspeed < 119 ~ 0,
                              windspeed >= 119 & windspeed <= 153 ~ 1,
                              windspeed > 153 & windspeed <= 177 ~ 2,
                              windspeed > 177 & windspeed <= 210 ~ 3,
                              windspeed > 210 & windspeed <= 251 ~ 4,
                              windspeed > 251 ~ 5),
           saffir = as.factor(saffir),
           saffir = fct_expand(saffir, "0", "1", "2", "3", "4", "5"))
  
  data_label_i <- st_intersection(data_ts_lines_i, data_eez_i)
  
  plot <- ggplot() +
    geom_sf(data = data_reefs_buffer %>% filter(territory == territory_i),
            fill = NA, color = "#363737", linewidth = 0.25, linetype = "dashed") +
    geom_sf(data = data_land_i) +
    geom_sf(data = data_crop, fill = "white", color = NA, alpha = 0.6) +
    geom_sf(data = data_land_i_bis) +
    geom_sf(data = data_ts_lines_i %>% filter(position <= 3), col = "#6c7a89", size = 0.25) +
    geom_sf(data = data_ts_lines_i %>% filter(position > 3), col = "#6c7a89", size = 0.25) +
    geom_sf(data = data_ts_points_i %>% filter(position <= 3), aes(col = saffir), size = 1, show.legend = TRUE) +
    geom_sf_label_repel(data = data_label_i %>% filter(position <= 3), aes(label = ts_name),
                        alpha = 0.75, size = 2.5, label.size  = NA) +
    scale_color_manual(breaks = c("0", "1", "2", "3", "4", "5"),
                       labels = c("Cat. 0", "Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                       values = c(palette_second[1:5], "black"),
                       name = "Saffir-Simpson category",
                       drop = FALSE) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1,
                                override.aes = list(size = 4))) +
    coord_sf(xlim = c(bbox_i[1], bbox_i[3]), ylim = c(bbox_i[2], bbox_i[4])) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.border = element_blank(),
          plot.background = element_blank(),
          legend.position = "top",
          legend.key = element_blank(),
          legend.direction = "horizontal")     
  
  if(territory_i %in% c("Cuba", "Panama")){
    
    plot <- plot +
      facet_wrap(~time_range, ncol = 2, drop = FALSE)      
    
  }else{
    
    plot <- plot +
      facet_wrap(~time_range, ncol = 3, drop = FALSE)
    
  }
  
  ggsave(filename = str_replace_all(paste0("figs/02_part-2/fig-3/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
                                    "---", "-"), width = 8, height = 6)
  
}

## 4.3 Map over the function ----

map(setdiff(unique(data_eez$territory),
            c("Overlapping claim Navassa Island: United States / Haiti / Jamaica",
              "Overlapping claim: Venezuela / Netherlands (Aruba) / Dominican Republic",
              "Overlapping claim: Colombia / Dominican Republic / Venezuela",
              "Overlapping claim: United States (Puerto Rico) / Dominican Republic",
              "Overlapping claim: Belize / Honduras",
              "Serrana Bank",
              "Quitasueño Bank")), # territories for which no chapter will be included
    ~plot_territories(territory_i = .))
