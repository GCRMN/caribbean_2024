# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(ggtext)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

load("data/02_misc/data-benthic.RData")

# 4. Main benthic categories ----

## 4.1 Transform data ----

data_benthic_cover <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID, eventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  # This is the case for datasets "0011", "0012", "0013", "0014", and "0043" at least
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                           category == "Coralline algae" ~ palette_second[3],
                           category == "Macroalgae" ~ palette_second[4],
                           category == "Turf algae" ~ palette_second[5]))

## 4.2 Caribbean region (density and trend) ----

plot_a <- ggplot(data = data_benthic_cover, aes(x = measurementValue, fill = color)) +
  geom_density() +
  scale_fill_identity() +
  facet_grid(~category) +
  theme_graph() + 
  lims(x = c(0, 100)) +
  labs(x = "Percentage cover", y = "Density") +
  theme(strip.text = element_text(hjust = 0.5, face = "bold"),
        strip.background = element_rect(color = NA, fill = "white"))

plot_b <- ggplot(data = data_benthic_cover, aes(x = year, y = measurementValue, color = color)) +
  geom_point(alpha = 0.1, color = "lightgrey") +
  scale_color_identity() +
  geom_smooth() +
  facet_grid(~category) +
  theme_graph() + 
  lims(y = c(-2, 100), x = c(1980, 2025)) +
  labs(y = "Percentage cover", x = "Year") +
  theme(strip.text = element_text(hjust = 0.5, face = "bold"),
        strip.background = element_rect(color = NA, fill = "white"))

plot_a + plot_b + plot_layout(ncol = 1)

ggsave(paste0("figs/06_additional/benthic-cover_region_density-trend.png"),
       width = 14, height = 8, dpi = fig_resolution)

## 4.3 Countries and territories (density) ----

ggplot(data = data_benthic_cover, aes(x = measurementValue, fill = color)) +
  geom_density() +
  scale_fill_identity() +
  facet_grid(territory~category, drop = FALSE) +
  theme_graph() + 
  lims(x = c(0, 100)) +
  labs(x = "Percentage cover") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
        strip.background = element_rect(color = NA, fill = "white"))

ggsave(paste0("figs/06_additional/benthic-cover_territory_density.png"),
       width = 10, height = 12, dpi = fig_resolution)

## 4.4 Countries and territories (trend) ----

ggplot(data = data_benthic_cover, aes(x = year, y = measurementValue, color = color)) +
  geom_point(alpha = 0.1, color = "lightgrey") +
  scale_color_identity() +
  geom_smooth() +
  facet_grid(territory~category, drop = FALSE) +
  theme_graph() + 
  lims(y = c(-2, 100), x = c(1980, 2025)) +
  labs(y = "Percentage cover", x = "Year") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
        strip.background = element_rect(color = NA, fill = "white"))

ggsave(paste0("figs/06_additional/benthic-cover_territory_trend.png"),
       width = 10, height = 12, dpi = fig_resolution)

# 5. Main hard coral genus ----

## 5.1 Transform data ----





