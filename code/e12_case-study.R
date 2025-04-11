# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(treemapify)
library(patchwork)
library(lwgeom)
library(readxl)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Panama ----

## 3.1 Map ----

data_land <- st_read("data/01_maps/02_clean/05_princeton/land.shp")

data_site <- tibble(lon = -82.24, lat = 9.33, site = "BOCAS DEL TORO") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

plot <- ggplot() +
  geom_sf(data = data_land) +
  geom_sf(data = data_site, fill = palette_first[2], color = "white", size = 8, shape = 21) +
  geom_sf_label(data = data_site, aes(label = site), fill = palette_first[2], size = 5, label.padding = unit(7, "pt"),
                color = "white", family = font_choose_graph, nudge_x = 0.45, nudge_y = 0.2) +
  coord_sf(xlim = c(-83,-81), ylim = c(8.7,10)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/03_case-studies/boca-del-torro_1.png", width = 6, height = 5)

## 3.2 Figure ----

data_boca <- read_xlsx("data/11_case-studies/boca-del-torro.xlsx") %>% 
  mutate(across(c("abundance", "reef_relief"), ~as.numeric(.x)))

ggplot(data = data_boca, aes(x = abundance, y = reef_relief)) +
  geom_smooth(method = "lm", fill = "lightgrey", color = palette_first[2]) +
  geom_point(fill = palette_first[2], size = 4, color = "white", shape = 21) +
  theme_graph() +
  theme(axis.line.y = element_line(linewidth = 0.4),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
        axis.title.x = element_markdown()) +
  lims(x = c(65, 127), y = c(0, 20)) +
  labs(x = "Mean fish abundance (ind.60 m<sup>-2</sup>)", y = "Maximum reef relief")

ggsave("figs/03_case-studies/boca-del-torro_2.png", width = 6, height = 5)
