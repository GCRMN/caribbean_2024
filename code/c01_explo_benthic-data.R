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
  filter(category %in% c("Hard coral", "Other fauna", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           eventID, decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Regenerate 0 values
  group_by(datasetID) %>% 
  complete(category,
           nesting(region, subregion, ecoregion, country, territory, area, locality,
                   habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                   year, month, day, eventDate),
           fill = list(measurementValue = 0)) %>%
  ungroup() %>% 
   # 4. Add colors 
  mutate(color = case_when(category == "Hard coral" ~ palette_second[1],
                           category == "Other fauna" ~ palette_second[2],
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

ggsave("figs/06_additional/benthic-cover_region_density-trend.png",
       width = 16, height = 8, dpi = fig_resolution)

## 4.3 Areas (density) ----

ggplot(data = data_benthic_cover, aes(x = measurementValue, fill = color)) +
  geom_density() +
  scale_fill_identity() +
  facet_grid(area~category, drop = FALSE, scale = "free_y") +
  theme_graph() + 
  lims(x = c(0, 100)) +
  labs(x = "Percentage cover") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
        strip.background = element_rect(color = NA, fill = "white"))

ggsave("figs/06_additional/benthic-cover_area_density.png",
       width = 15, height = 28, dpi = fig_resolution)

## 4.4 Areas (minimum values) ----

data_benthic_cover %>%
  group_by(category, area) %>% 
  summarise(min = min(measurementValue)) %>% 
  ungroup() %>% 
  arrange(area) %>% 
  mutate(color = case_when(min == 0 ~ "#89c4f4",
                           min >= 0 ~ "#f1828d",
                           is.na(min) ~ "lightgrey")) %>% 
  ggplot(., aes(x = category, y = area)) +
    geom_tile(aes(fill = color)) +
    scale_fill_identity() +
    scale_y_discrete(limits = rev) +
    geom_text(aes(label = round(min, 1))) +
    theme_graph() + 
    labs(x = NULL, y = NULL)

ggsave("figs/06_additional/benthic-cover_area_min-value.png",
       width = 12, height = 20, dpi = fig_resolution)

## 4.5 Areas (trend) ----

ggplot(data = data_benthic_cover, aes(x = year, y = measurementValue, color = color)) +
  geom_point(alpha = 0.1, color = "lightgrey") +
  scale_color_identity() +
  geom_smooth() +
  facet_grid(area~category, drop = FALSE) +
  theme_graph() + 
  lims(y = c(-2, 100), x = c(1980, 2025)) +
  labs(y = "Percentage cover", x = "Year") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
        strip.background = element_rect(color = NA, fill = "white"))

ggsave("figs/06_additional/benthic-cover_area_trend.png",
       width = 15, height = 28, dpi = fig_resolution)

## 4.6 DatasetID (density) ----

ggplot(data = data_benthic_cover, aes(x = measurementValue, fill = color)) +
  geom_density() +
  scale_fill_identity() +
  facet_grid(datasetID~category, drop = FALSE, scale = "free_y") +
  theme_graph() + 
  lims(x = c(0, 100)) +
  labs(x = "Percentage cover") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
        strip.background = element_rect(color = NA, fill = "white"))

ggsave("figs/06_additional/benthic-cover_dataset_density.png",
       width = 15, height = 28, dpi = fig_resolution)

## 4.7 DatasetID (minimum values) ----

data_benthic_cover %>%
  group_by(category, datasetID) %>% 
  summarise(min = min(measurementValue)) %>% 
  ungroup() %>% 
  arrange(datasetID) %>% 
  mutate(color = case_when(min == 0 ~ "#89c4f4",
                           min >= 0 ~ "#f1828d",
                           is.na(min) ~ "lightgrey")) %>% 
  ggplot(., aes(x = category, y = datasetID)) +
  geom_tile(aes(fill = color)) +
  scale_fill_identity() +
  scale_y_discrete(limits = rev) +
  geom_text(aes(label = round(min, 1))) +
  theme_graph() + 
  labs(x = NULL, y = NULL)

ggsave("figs/06_additional/benthic-cover_dataset_min-value.png",
       width = 12, height = 20, dpi = fig_resolution)

## 4.8 DatasetID (trend) ----

ggplot(data = data_benthic_cover, aes(x = year, y = measurementValue, color = color)) +
  geom_point(alpha = 0.1, color = "lightgrey") +
  scale_color_identity() +
  geom_smooth() +
  facet_grid(datasetID~category, drop = FALSE) +
  theme_graph() + 
  lims(y = c(-2, 100), x = c(1980, 2025)) +
  labs(y = "Percentage cover", x = "Year") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
        strip.background = element_rect(color = NA, fill = "white"))

ggsave("figs/06_additional/benthic-cover_dataset_trend.png",
       width = 15, height = 28, dpi = fig_resolution)

# 5. Main hard coral genera ----

## 5.1 Main genera per percentage cover ----

data_benthic_cover <- data_benthic %>% 
  filter(category == "Hard coral" & !is.na(genus)) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID, eventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, genus) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  group_by(genus) %>% 
  summarise(measurementValue = mean(measurementValue))

ggplot(data = data_benthic_cover, aes(x = fct_reorder(genus, measurementValue), y = measurementValue)) +
  geom_bar(stat = "identity", fill = palette_first[3], width = 0.8) +
  coord_flip() +
  labs(y = "Average percentage cover", x = NULL) +
  theme_graph() +
  theme(axis.text.y = element_text(face = "italic"))

ggsave("figs/06_additional/benthic-cover_hcc-genera_barplot.png",
       width = 6, height = 10, dpi = fig_resolution)

## 5.2 Caribbean region (density and trend) ----

data_benthic_cover <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  filter(family %in% c("Acroporidae", "Merulinidae")) %>% 
  mutate(category = family) %>% 
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  group_by(datasetID, region, subregion, ecoregion, country, territory, area, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Regenerate 0 values
  group_by(datasetID) %>% 
  complete(category,
           nesting(region, subregion, ecoregion, country, territory, area, locality,
                   habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                   year, month, day, eventDate),
           fill = list(measurementValue = 0)) %>%
  ungroup() %>% 
  # 4. Add colors 
  mutate(color = case_when(category == "Acroporidae" ~ palette_second[1],
                           category == "Merulinidae" ~ palette_second[2]))

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

ggsave("figs/06_additional/benthic-cover_region_density-trend_fam.png",
       width = 8, height = 8, dpi = fig_resolution)
