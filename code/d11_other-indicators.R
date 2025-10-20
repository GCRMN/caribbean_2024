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

## 3.1 Reef area by Caribbean area ----

data_reefs <- st_read("data/01_maps/02_clean/02_reefs/reefs.shp")

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

## 3.2 Reef area by GCRMN regions ----

data_reefs <- st_read("data/01_maps/01_raw/01_reefs/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

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

ggsave(filename = "figs/01_part-1/fig-5_raw.png", height = 5, width = 10, dpi = fig_resolution)

## 3.5 Export the table ----

data_reef_area_caribbean %>% 
  mutate(reef_area_rel_caribbean = (reef_area_abs*100)/sum(reef_area_abs),
         reef_area_rel_world = (100*reef_area_abs)/(as.numeric(st_area(data_reefs))*1e-6)) %>% 
  arrange(area) %>% 
  bind_rows(., tibble(area = "Entire Caribbean region",
                      reef_area_abs = sum(data_reef_area_caribbean$reef_area_abs),
                      reef_area_rel_caribbean = 100,
                      reef_area_rel_world = sum(.$reef_area_rel_world))) %>% 
  mutate(reef_area_abs = format(round(reef_area_abs, 0), big.mark = ",", scientific = FALSE),
         across(c(reef_area_rel_caribbean, reef_area_rel_world), ~format(round(.x, 3), nsmall = 3))) %>% 
  openxlsx::write.xlsx(., file = "figs/01_part-1/tbl-1.xlsx")

# 4. Human population ----

## 4.1 Human population per EEZ / area ----

data_population_eez <- read.csv("data/02_misc/ind_human-pop_eez.csv") %>% 
  mutate(year = as.numeric(str_sub(date, 1, 4)),
         area = case_when(area == "Cura�ao" ~ "Curaçao",
                          area == "Saint-Barth�lemy" ~ "Saint-Barthélemy",
                          TRUE ~ area)) %>% 
  select(-date)

data_population_eez <- data_population_eez %>% 
  # Duplicate values for United States (FGB and Florida)
  filter(area == "United States") %>% 
  mutate(area = "Flower Garden Banks") %>% 
  bind_rows(., data_population_eez %>% 
              filter(area == "United States") %>% 
              mutate(area = "Florida")) %>% 
  # Duplicate values for Mexico (Caribbean Sea and Gulf of Mexico)
  bind_rows(., data_population_eez %>% 
              filter(area == "Mexico") %>% 
              mutate(area = "Mexico (Caribbean Sea)")) %>% 
  bind_rows(., data_population_eez %>% 
              filter(area == "Mexico") %>% 
              mutate(area = "Mexico (Gulf of Mexico)")) %>% 
  bind_rows(., data_population_eez %>% 
              filter(!(area %in% c("Mexico", "United States")))) %>% 
  pivot_wider(names_from = year, values_from = sum, names_prefix = "pop_eez_")

## 4.2 Human population from 20 km reef buffer ----

data_population_reef <- read.csv("data/02_misc/ind_human-pop_20km.csv") %>% 
  mutate(year = as.numeric(str_sub(date, 1, 4)),
         area = case_when(area == "Cura�ao" ~ "Curaçao",
                          area == "Saint-Barth�lemy" ~ "Saint-Barthélemy",
                          TRUE ~ area)) %>% 
  select(-date) %>% 
  pivot_wider(names_from = year, values_from = sum, names_prefix = "pop_reef_")

## 4.3 Calculate population indicators ----

### 4.3.1 Join population EEZ and reef buffer ----

data_population <- left_join(data_population_eez, data_population_reef) %>% 
  filter(!(area %in% c("Navassa Island"))) %>% 
  arrange(area)

### 4.3.2 Add the total for the Caribbean region ----

data_population <- bind_rows(data_population, data_population %>% 
                               summarise(across(c("pop_reef_2000", "pop_reef_2020", "pop_eez_2000", "pop_eez_2020"),
                                                ~sum(.x))) %>% 
                               mutate(area = "Entire Caribbean region"))

### 4.3.3 Calculate indicators ----

data_population <- data_population %>% 
  mutate(pop_reef_change = ((pop_reef_2020-pop_reef_2000)/pop_reef_2000)*100,
         pop_percent = (pop_reef_2020*100)/pop_eez_2020) %>% 
  mutate(across(c("pop_reef_change", "pop_percent"), ~if_else(is.na(.x), 0, .x))) %>% 
  select(area, pop_reef_2020, pop_percent, pop_reef_change) %>% 
  # Reformat the data
  mutate(pop_reef_2020 = format(round(pop_reef_2020, 0), big.mark = ",", scientific = FALSE),
         pop_percent = format(round(pop_percent, 2), nsmall = 2),
         pop_reef_change = format(round(pop_reef_change, 2), nsmall = 2))

## 4.4 Export the table ----

openxlsx::write.xlsx(data_population, file = "figs/01_part-1/tbl-2.xlsx")

## 4.5 Caribbean population plot ----

data_population_reef %>% 
  pivot_longer(2:ncol(.), names_to = "year", values_to = "population") %>% 
  mutate(year = as.numeric(str_sub(year, 10, 14)),
         area_type = case_when(area %in% c("Haiti", "Cuba", "Florida", "Dominican Republic") ~ area,
                               TRUE ~ "Other territories"),
         population = population*1e-06) %>% # Convert to million
  group_by(area_type, year) %>% 
  summarise(population = sum(population, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(data = ., aes(x = year, y = population, fill = area_type, label = area_type)) +
  #geom_area(show.legend = TRUE, color = "white", linewidth = 0.25) + # To know where to place text
  geom_area(show.legend = FALSE, color = "white", linewidth = 0.25) +
  scale_fill_manual(values = rev(palette_first)) +
  labs(x = "Year", y = "Inhabitants (millions)") +
  theme_graph() +
  lims(y = c(0, 60)) +
  annotate(geom = "text", label = "Cuba", x = 2019, y = 43, 
           family = font_choose_graph, color = "white", hjust = 1, size = 3) +
  annotate(geom = "text", label = "DR", x = 2019, y = 37, 
           family = font_choose_graph, color = "white", hjust = 1, size = 3) +
  annotate(geom = "text", label = "Florida", x = 2019, y = 31, 
           family = font_choose_graph, color = "white", hjust = 1, size = 3) +
  annotate(geom = "text", label = "Haiti", x = 2019, y = 23.5, 
           family = font_choose_graph, color = "white", hjust = 1, size = 3) +
  annotate(geom = "text", label = "Other countries and territories", x = 2019, y = 8, 
           family = font_choose_graph, color = "black", hjust = 1, size = 3)

ggsave("figs/01_part-1/fig-8.png", height = 4, width = 5, dpi = fig_resolution)
