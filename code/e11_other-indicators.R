# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(treemapify)
library(patchwork)
library(lwgeom)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Relative coral reef extent of the Caribbean ----

## 3.1 Load and transform data ----

data_reefs <- st_read("data/01_maps/01_raw/01_reefs/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

## 3.2 Reef area by Caribbean area ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_reef_area_caribbean <- st_intersection(data_area, data_reefs) %>%  
  group_by(area) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6) %>% 
  arrange(-reef_area_abs)

plot_b <- ggplot(data = data_reef_area_caribbean, aes(area = reef_area_abs, fill = reef_area_abs, label = area)) +
  geom_treemap(show.legend = FALSE, color = "white", size = 2) +
  geom_treemap_text(color = "white", place = "centre", reflow = TRUE, family = font_choose_graph) +
  scale_fill_gradientn(colours = palette_first[3:5]) +
  theme_graph()

ggsave(filename = "figs/01_part-1/fig-5b.png", plot = plot_b, height = 5, width = 5, dpi = fig_resolution)

## 3.3 Reef area by GCRMN regions ----

load("data/01_maps/02_clean/01_gcrmn-regions/gcrmn_regions.RData")

data_reef_area_gcrmn <- st_intersection(data_gcrmn_regions, data_reefs) %>%  
  group_by(region) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6,
         region = str_replace_all(region, "EAS", "East Asian Seas"),
         color = case_when(region == "Caribbean" ~ palette_first[3],
                           TRUE ~ palette_first[4]),
         color_text = case_when(region == "Caribbean" ~ "white",
                                TRUE ~ "white"))

plot_a <- ggplot(data = data_reef_area_gcrmn, aes(area = reef_area_abs, fill = color, label = region)) +
  geom_treemap(show.legend = FALSE, color = "white", size = 2, start = "bottomleft") +
  geom_treemap_text(aes(color = color_text), place = "centre", reflow = TRUE,
                    family = font_choose_graph, start = "bottomleft") +
  scale_fill_identity() +
  scale_color_identity() +
  theme_graph()

ggsave(filename = "figs/01_part-1/fig-5a.png", plot = plot_a, height = 5, width = 5, dpi = fig_resolution)

## 3.4 Combine the two figures ----

(plot_a + labs(title = "A") + theme(plot.margin = unit(c(0.25, 0.75, 0.25, 0), "cm"),
                                    plot.title = element_text(size = 18))) +
  (plot_b + labs(title = "B") + theme(plot.margin = unit(c(0.25, 0, 0.25, 0.75), "cm"),
                                      plot.title = element_text(size = 18)))

ggsave(filename = "figs/01_part-1/fig-5.png", height = 5, width = 10, dpi = fig_resolution)

## 3.5 Export the table ----

data_reef_area_caribbean %>% 
  mutate(reef_area_rel = (reef_area_abs*100)/sum(reef_area_abs),
         across(c(reef_area_abs, reef_area_rel), ~round(.x, 2))) %>% 
  arrange(area) %>% 
  bind_rows(., tibble(area = "Entire Caribbean region",
                      reef_area_abs = sum(data_reef_area_caribbean$reef_area_abs),
                      reef_area_rel = 100)) %>% 
  openxlsx::write.xlsx(., file = "figs/01_part-1/tbl-1.xlsx")
