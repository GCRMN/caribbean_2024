# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(ggrepel)
library(scales)
library(zoo)
library(Kendall)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/combine_model_data.R")
source("code/function/extract_mannkendall.R")
source("code/function/plot_vimp.R")
source("code/function/plot_pdp.R")
source("code/function/plot_trends.R")
source("code/function/plot_residuals.R")
source("code/function/plot_pred_obs.R")
source("code/function/combine_plot_trends.R")
source("code/function/plot_prediction_map.R")
source("code/function/plot_raw_trends.R")
source("code/function/export_raw_trends.R")

theme_set(theme_graph())

# 3. Figure trends raw ----

load("data/09_model-data/data_benthic_prepared.RData")

data_benthic <- data_benthic %>% 
  mutate(color = case_when(category == "Hard coral" ~ "#c44d56",
                           category == "Algae" ~ "#16a085",
                           category == "Other fauna" ~ "#714d69",
                           category == "Macroalgae" ~ "#03a678",
                           category == "Turf algae" ~ "#26a65b",
                           category == "Coralline algae" ~ "#C5987D",
                           category == "Acropora" ~ "#e08283",
                           category == "Orbicella" ~ "#c44d56",
                           category == "Porites" ~ "#a37c82"),
         text_title = case_when(category == "Hard coral" ~ 
                                  glue("**A.**<span style='color:{color}'> {category}</span>"),
                                category == "Algae" ~ 
                                  glue("**B.**<span style='color:{color}'> {category}</span>"),
                                category == "Other fauna" ~ 
                                  glue("**C.**<span style='color:{color}'> {category}</span>"),
                                
                                category == "Coralline algae" ~ 
                                  glue("**A.**<span style='color:{color}'> {category}</span>"),
                                category == "Macroalgae" ~ 
                                  glue("**B.**<span style='color:{color}'> {category}</span>"),
                                category == "Turf algae" ~ 
                                  glue("**C.**<span style='color:{color}'> {category}</span>"),
                                
                                category == "Acropora" ~ 
                                  glue("**A.***<span style='color:{color}'> {category}</span>*"),
                                category == "Orbicella" ~ 
                                  glue("**B.***<span style='color:{color}'> {category}</span>*"),
                                category == "Porites" ~ 
                                  glue("**C.***<span style='color:{color}'> {category}</span>*")))

data_benthic <- data_benthic %>% 
  group_by(year, area, category, color, text_title) %>% 
  summarise(mean = mean(measurementValue),
            sd = sd(measurementValue)) %>% 
  ungroup() %>% 
  mutate(ymin = mean - sd,
         ymin = ifelse(ymin < 0, 0, ymin),
         ymax = mean + sd) %>% 
  bind_rows(., data_benthic %>% 
              group_by(year, category, color, text_title) %>% 
              summarise(mean = mean(measurementValue),
                        sd = sd(measurementValue)) %>% 
              ungroup() %>% 
              mutate(ymin = mean - sd,
                     ymin = ifelse(ymin < 0, 0, ymin),
                     ymax = mean + sd,
                     area = "Caribbean")) %>% 
  complete(year, category, nesting(area), fill = list(mean = NA, sd = NA)) %>% 
  select(-color, -text_title) %>% 
  left_join(., data_benthic %>% 
              select(category, text_title, color) %>% 
              distinct()) %>% 
  drop_na(area)

A <- plot_raw_trends(category_i = "Hard coral",
                     data_benthic_i = data_benthic %>%
                       filter(area == "Caribbean")) +
  lims(x = c(1970, 2025), y = c(0, 100)) +
  annotate("segment", x = 1970, xend = 1980, y = 35, color = "#c44d56", linetype = "dashed") +
  annotate("rect", xmin = 1970, xmax = 1980, ymin = 30, ymax = 40, fill = "#c44d56", alpha = 0.2) +
  theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank(),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"))

B <- plot_raw_trends(category_i = "Algae",
                     data_benthic_i = data_benthic %>%
                       filter(area == "Caribbean"))  +
  lims(x = c(1970, 2025), y = c(0, 100)) +
  annotate("segment", x = 1970, xend = 1980, y = 7, color = "#03a678", linetype = "dashed") +
  annotate("rect", xmin = 1970, xmax = 1980, ymin = 2, ymax = 12, fill = "#03a678", alpha = 0.2) +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"))

A + B + 
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  ))

ggsave("figs/00_misc/fig-2.png", dpi = 600, width = 11, height = 5, bg = "transparent")
ggsave("figs/00_misc/fig-2.svg", dpi = 600, width = 11, height = 5, bg = "transparent")

# 4. Map ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(terra)
library(tidyterra)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")
theme_set(theme_map())

# 3. Load data ----

## 3.1 Country boundaries ----

data_land <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

## 3.2 Reefs ----

data_reefs <- read_sf("data/01_maps/02_clean/02_reefs/reefs.shp")

## 3.3 Background RGB tif ----

data_tif <- rast("data/01_maps/01_raw/04_natural-earth/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_tif <- crop(data_tif, data_crop)

## 3.4 EEZ ----

data_eez <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_land_cropped <- st_intersection(data_land, data_crop)

data_eez <- st_difference(data_eez, st_union(data_land_cropped))

data_land_boundaries <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_boundary_lines_land/ne_10m_admin_0_boundary_lines_land.shp")

## 3.5 Monitoring sites ----

load("data/09_model-data/data_benthic_prepared.RData")

data_sites <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 4. Make the basic regional map ----

plot_map <- ggplot() +
  #geom_spatraster_rgb(data = data_tif, maxcell = 5e+07) +
  #geom_sf(data = data_crop, alpha = 0.3, fill = "white") +
  geom_sf(data = data_land, fill = "lightgrey", color = "white", linewidth = 0.15) +
  geom_sf(data = data_reefs, fill = "#d91e18", color = "#d91e18") +
  geom_sf(data = data_eez, color = "white", fill = NA, linewidth = 0.15) +
  geom_sf(data = data_sites, size = 0.2, color = "#9b59b6") +
  geom_sf(data = data_land_boundaries, color = "white", fill = NA, linewidth = 0.15) +
  coord_sf(xlim = c(-110, -45), ylim = c(-3, 45)) +
    annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                     text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"), line_col = "black",
                     pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black")) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave("figs/00_misc/fig-1.png", plot_map, width = 7.25, height = 4.75, dpi = 600, bg = "transparent")
ggsave("figs/00_misc/fig-1.svg", plot_map, width = 7.25, height = 4.75, dpi = 600, bg = "transparent")
