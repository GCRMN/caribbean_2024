# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(readxl)
library(ggtext)
library(sf)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Data for Sint-Maarten ----

data_sintmarteen <- read.csv2("data/12_fish-data/fish_data_SintMaarten.csv", skip = 1) %>% 
  mutate(year = str_replace_all(year, "2017_post_Irma", "2017"),
         across(c(year, biomass.g.100m2, site_latitude, site_longitude), ~as.numeric(.x)),
         dataset = "sint_marteen") %>% 
  filter(family %in% c("Acanthuridae", "Epinephelidae", "Scaridae")) %>% 
  filter(year == min(year) | year == max(year)) %>% 
  mutate(year = as.character(year)) %>% 
  rename(biomass = "biomass.g.100m2", mpa = site_within_mpa) %>% 
  mutate(biomass = biomass/1000)

ggplot(data = data_sintmarteen, aes(x = year, y = biomass, fill = as.factor(mpa))) +
  geom_boxplot(width = 0.6, outliers = FALSE, show.legend = FALSE) +
  facet_wrap(~family, scales = "free_y", ncol = 1) +
  labs(x = "Year", y = bquote("Biomass (kg.100/"~m^2*")")) +
  scale_fill_manual(values = c("no" = "#d35f5fff", "yes" = "#2C5D96")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, NA)) +
  theme_graph() +
  theme(strip.background = element_blank(),
        text = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(hjust = 0, size = 13, face = "bold"),
        panel.spacing = unit(1.3, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/01_part-1/fig-15a.png", width = 3.4, height = 8)

# 4. Data for Curaçao ----

data_curacao <- read_xlsx("data/12_fish-data/GCRMN_Curacao_fishdata_2015_2023.xlsx", skip = 1) %>% 
  mutate(dataset = "curacao") %>% 
  filter(family %in% c("Acanthuridae", "Epinephelidae", "Scaridae")) %>% 
  filter(year == min(year) | year == max(year)) %>% 
  mutate(year = as.character(year)) %>% 
  rename(biomass = "biomass g/100m2", mpa = site_within_mpa) %>% 
  mutate(biomass = biomass/1000)

ggplot(data = data_curacao, aes(x = year, y = biomass, fill = as.factor(mpa))) +
  geom_boxplot(width = 0.6, outliers = FALSE, show.legend = FALSE) +
  facet_wrap(~family, scales = "free_y", ncol = 3) +
  labs(x = "Year", y = bquote("Biomass (kg.100/"~m^2*")")) +
  scale_fill_manual(values = c("no" = "#d35f5fff", "yes" = "#2C5D96")) + 
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, NA)) +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15, face = "bold"),
        panel.spacing = unit(5, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/01_part-1/fig-15b.png", width = 12, height = 3.5)

# 5. Data for MAR ----

data_hri <- read_xlsx("data/12_fish-data/template_fish_data_Rev_final_clean_four_families_HRI.xlsx", skip = 1, sheet = 1) %>% 
  mutate(dataset = "hri") %>% 
  select(-Country) %>% 
  filter(family %in% c("Acanthuridae", "Epinephelidae", "Scaridae")) %>% 
  filter(year == min(year) | year == max(year)) %>% 
  mutate(year = as.character(year)) %>% 
  rename(biomass = "biomass g/100m2", mpa = site_within_mpa) %>% 
  mutate(biomass = biomass/1000)

ggplot(data = data_hri, aes(x = year, y = biomass, fill = as.factor(mpa))) +
  geom_boxplot(width = 0.6, outliers = FALSE, show.legend = FALSE) +
  facet_wrap(~family, scales = "free_y", ncol = 1) +
  labs(x = "Year", y = bquote("Biomass (kg.100/"~m^2*")")) +
  scale_fill_manual(values = c("no" = "#d35f5fff", "yes" = "#2C5D96")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, NA)) +
  theme_graph() +
  theme(strip.background = element_blank(),
        text = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(hjust = 0, size = 13, face = "bold"),
        panel.spacing = unit(1.3, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/01_part-1/fig-15c.png", width = 3.4, height = 8)

# 6. FUNDEMAR ----

data_fundemar <- read_xlsx("data/12_fish-data/fish_data_FUNDEMAR.xlsx", skip = 1, sheet = 2) %>% 
  mutate(dataset = "fundemar") %>% 
  filter(family %in% c("Acanthuridae", "Epinephelidae", "Scaridae")) %>% 
  filter(year == min(year) | year == max(year)) %>% 
  mutate(year = as.character(year)) %>% 
  rename(biomass = "biomass g/100m2", mpa = site_within_mpa) %>% 
  mutate(biomass = biomass/1000)

ggplot(data = data_fundemar, aes(x = year, y = biomass, fill = as.factor(mpa))) +
  geom_boxplot(width = 0.3, outliers = FALSE, show.legend = FALSE) +
  facet_wrap(~family, scales = "free_y", ncol = 3) +
  labs(x = "Year", y = bquote("Biomass (kg.100/"~m^2*")")) +
  scale_fill_manual(values = c("no" = "#d35f5fff", "yes" = "#2C5D96")) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, NA)) +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15, face = "bold"),
        panel.spacing = unit(5, "lines"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/01_part-1/fig-15d.png", width = 12, height = 3.5, bg = "transparent")

# 7. Caribbean map ----

data_coordinates <- data_sintmarteen %>% 
  select(dataset, site_latitude, site_longitude) %>% 
  bind_rows(., data_curacao %>% 
              select(dataset, site_latitude, site_longitude)) %>% 
  bind_rows(., data_hri %>% 
              select(dataset, site_latitude, site_longitude)) %>% 
  bind_rows(., data_fundemar %>% 
              select(dataset, site_latitude, site_longitude)) %>% 
  distinct() %>% 
  st_as_sf(., coords = c("site_longitude", "site_latitude"), crs = 4326)

data_land_ne <- read_sf("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

ggplot() +
  geom_sf(data = data_land_ne, linewidth = 0.1) +
  geom_sf(data = data_coordinates, color = "#42b9bc", size = 2) +
  coord_sf(xlim = c(-105, -50), ylim = c(6, 38), expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5))

ggsave("figs/01_part-1/fig-15e.png", width = 8.8, height = 5.6, dpi = 300)

# 8. Get number of sites ----

data_coordinates %>% 
  group_by(dataset) %>% 
  count()
