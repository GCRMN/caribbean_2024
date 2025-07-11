# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map.R")
source("code/function/limits_region.R")
source("code/function/data_descriptors.R")

# 3. Load cyclones ----

load("data/07_cyclones/01_cyclones_lines.RData")
load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  filter(saffir > 0) %>% 
  mutate(saffir = as.factor(saffir)) %>% 
  right_join(data_ts_lines, .)

# 4. Map of cyclones in the region ----

## 4.1 Load data ----

data_land <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_eez <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_land_cropped <- st_intersection(data_land, data_crop)

data_eez <- st_difference(data_eez, st_union(data_land_cropped))

data_land_boundaries <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_boundary_lines_land/ne_10m_admin_0_boundary_lines_land.shp")

## 4.2 Map of cyclone trajectories ----

plot <- ggplot() +
  geom_sf(data = data_cyclones %>% arrange(saffir), aes(color = saffir),
          alpha = 0.75, linewidth = 0.3, show.legend = "line") +
  scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                     labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                     values = c(palette_second[2:5], "black"),
                     name = "Saffir-Simpson\ncategory",
                     drop = FALSE) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(linewidth = 1))) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_sf(data = data_eez, color = "#57add2", fill = NA, linewidth = 0.15) +
  geom_sf(data = data_land, color = "#57add2", fill = "#d9d9d9", linewidth = 0.05) +
  geom_sf(data = data_land_boundaries, color = "#979796", fill = NA, linewidth = 0.15) +
  limits_region() +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                   text_cex = 0.6, style = "bar", line_width = 1,  height = unit(0.04, "cm"), line_col = "black",
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black")) +
  theme_map() +
  theme(legend.position = "inside",
        legend.direction = "vertical",
        legend.background = element_rect(color = "black", linewidth = 0.1, fill = "#fbfbfb"),
        legend.title = element_text(size = 7, hjust = 0),
        legend.text = element_text(size = 6, margin = margin(t = 0)),
        legend.key.size = unit(0.4, "cm"),
        legend.position.inside = c(0.1,0.21))

ggsave(filename = "figs/01_part-1/fig-3b.png", plot = plot,
       width = 7.25, height = 4.75, dpi = fig_resolution)

## 4.3 Map of cyclone occurrence (hex grid) ----

### 4.3.1 Create the hex grid ----

data_grid_cyclones <- st_make_grid(data_crop, cellsize = 1, square = FALSE) %>% 
  st_as_sf(crs = 4326)

data_grid <- st_make_grid(data_crop, cellsize = 1, square = FALSE) %>% 
  st_as_sf(crs = 4326) %>% 
  mutate(cell_id = row_number())

### 4.3.2 Count number of unique cyclones crossing each hex cell ----

data_grid_cyclones <- st_join(data_grid, data_cyclones, .predicates = st_intersects) %>% 
  select(cell_id, ts_id) %>% 
  distinct() %>% 
  group_by(cell_id) %>% 
  summarise(n = n()) %>% 
  ungroup()

### 4.3.3 Extract only hex cells located on coral reefs ----

data_reefs <- st_read("data/01_maps/02_clean/02_reefs/reefs.shp")

data_grid_cyclones <- st_join(data_grid_cyclones, data_reefs, .predicates = st_intersects) %>% 
  drop_na(area)

### 4.3.4 Make the plot ----

plot <- ggplot() +
  geom_sf(data = data_grid_cyclones, aes(fill = n), alpha = 1, color = "white") +
  scale_fill_gradientn(colours = c("#faedd6ff", "#fac484", "#ce6693", "#a059a0", "#5c53a5"),
                       breaks = c(0, 5, 10, 15, 20), limits = c(0, 20), labels = c("0", "5", "10", "15", "20"),
                       name = "Number of\nhurricanes") +
  geom_sf(data = data_eez, color = "#57add2", fill = NA, linewidth = 0.15) +
  geom_sf(data = data_land, color = "#57add2", fill = "#d9d9d9", linewidth = 0.05) +
  geom_sf(data = data_land_boundaries, color = "#979796", fill = NA, linewidth = 0.15) +
  limits_region() +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                   text_cex = 0.6, style = "bar", line_width = 1,  height = unit(0.04, "cm"), line_col = "black",
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black")) +
  theme_map() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "inside",
        legend.direction = "vertical",
        legend.background = element_rect(color = "black", linewidth = 0.1, fill = "#fbfbfb"),
        legend.title = element_text(size = 7, hjust = 0),
        legend.text = element_text(size = 6),
        legend.frame = element_rect(color = "black", linewidth = 0.2),
        legend.ticks = element_line(color = "black", linewidth = 0.2),
        legend.key.width = unit(0.3, "cm"),
        legend.key.size = unit(0.35, "cm"),
        legend.margin = margin(8,14,8,12),
        legend.position.inside = c(0.1,0.21))

ggsave(filename = "figs/01_part-1/fig-3.png", plot = plot,
       width = 7.25, height = 4.75, dpi = fig_resolution)

rm(data_crop, data_eez, data_land, data_land_cropped, plot, data_grid, data_grid_cyclones)

# 5. Comparison of cyclones occurrence ----

## 5.1 Transform data ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp") %>% 
  filter(!(area %in% c("Navassa Island")))

data_cyclones <- data_cyclones %>% 
  st_drop_geometry() %>% 
  group_by(area, saffir) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(area) %>% 
  mutate(n_tot = sum(n)) %>% 
  ungroup() %>% 
  bind_rows(., tibble(area = setdiff(data_area$area, data_cyclones$area),
                      n = rep(0, length(setdiff(data_area$area, data_cyclones$area))),
                      n_tot = n)) %>% 
  mutate(area = str_replace_all(area, c("Islands" = "Isl.",
                                        " and the " = " & ",
                                        " and " = " & ",
                                        "United States" = "U.S.",
                                        "Saint " = "St. ")))

## 5.2 Make the plot ----

ggplot(data = data_cyclones, aes(x = n, y = fct_reorder(area, n_tot), fill = saffir)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                    labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                    values = c(palette_second[2:5], "black"),
                    name = "Saffir-Simpson\ncategory\n",
                    drop = FALSE) +
  theme_graph() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.65, 0.2),
        legend.direction = "vertical",
        legend.background = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 75)) +
  labs(x = "Number of hurricanes", y = NULL)

## 5.3 Save the plot ----

ggsave(filename = "figs/01_part-1/fig-7.png", width = 6, height = 12, dpi = fig_resolution)

# 6. Top ten most severe cyclones ----

load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones_top <- data_cyclones %>% 
  mutate(year = year(time)) %>% 
  select(ts_id, ts_name, windspeed, year, saffir) %>% 
  distinct() %>% 
  group_by(ts_id) %>% 
  filter(windspeed == max(windspeed)) %>% 
  ungroup() %>% 
  arrange(-windspeed) %>% 
  slice(1:10) %>% 
  mutate(ts_name = str_to_sentence(ts_name),
         label = paste0(round(windspeed), " km.h<sup>-1</sup> (**", year, "**)"))

ggplot(data = data_cyclones_top, aes(x = fct_reorder(ts_name, -desc(windspeed)), y = windspeed, label = label)) +
  geom_bar(stat = "identity", width = 0.5, fill = palette_first[3]) +
  geom_richtext(hjust = 1.2, label.color = NA, color = "white", fill = NA) +
  coord_flip() +
  labs(y = "Wind speed (km.h<sup>-1</sup>)", x = NULL) +
  theme_graph() +
  theme(axis.title = element_markdown())

ggsave("figs/05_supp-mat/top-ten-hurricanes.png", width = 5, height = 7)

# 7. Key numbers ----

load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1)

## 7.1 Total number of cyclones ----

length(unique(data_cyclones$ts_id))

## 7.2 Max number of cyclones ----

data_cyclones %>% 
  select(ts_id, area) %>% 
  distinct() %>% 
  group_by(area) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n == max(n))

## 7.3 Highest windspeed ----

data_cyclones %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  arrange(desc(windspeed))

## 7.4 Mean number of cyclone per year ----

data_cyclones_year <- data_cyclones %>% 
  mutate(year = year(time)) %>% 
  select(year, ts_id) %>% 
  distinct() %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  complete(year = seq(1980, 2024, 1), fill = list(n = 0))

mean(data_cyclones_year$n)
sd(data_cyclones_year$n)
