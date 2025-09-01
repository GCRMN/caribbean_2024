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

# 4. Dominican Republic and Bonaire ----

## 4.1 Map ----


## 4.2 Figure ----

data_bonaire <- read_xlsx("data/11_case-studies/Summary DR-Bonaire comparison_refformatted.xlsx")

ggplot(data = data_bonaire, aes(x = year, y = mean, color = location)) +
  geom_line(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean-standard_error, ymax = mean+standard_error), width = 0.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  lims(y = c(0, NA)) +
  geom_vline(xintercept = 2010) +
  theme_graph() +
  facet_wrap(~variable, scales = "free_y")

plot_a <- ggplot(data = data_bonaire %>% filter(variable == "coral"), aes(x = year, y = mean, color = location)) +
  geom_line(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean-standard_error, ymax = mean+standard_error), width = 0.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  lims(y = c(0, NA)) +
  geom_vline(xintercept = 2010) +
  theme_graph() +
  labs(x = "Year", y = "Hard coral cover (%)", title = "A")

plot_b <- ggplot(data = data_bonaire %>% filter(variable == "macroalgae"), aes(x = year, y = mean, color = location)) +
  geom_line(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean-standard_error, ymax = mean+standard_error), width = 0.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  lims(y = c(0, 30)) +
  geom_vline(xintercept = 2010) +
  theme_graph() +
  labs(x = "Year", y = "Macroalgae cover (%)", title = "B")

plot_c <- ggplot(data = data_bonaire %>% filter(variable == "parrotfish"), aes(x = year, y = mean, color = location)) +
  geom_line(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean-standard_error, ymax = mean+standard_error), width = 0.5) +
  geom_point(shape = 21, fill = "white", size = 2) +
  lims(y = c(0, NA)) +
  geom_vline(xintercept = 2010) +
  theme_graph() +
  labs(x = "Year", y = "Parrotfish biomass", title = "D")

plot_a + plot_b + plot_c + plot_layout(guides = "collect") + theme(legend.direction = "vertical")

# 5. SocMon ----

## 5.1 Figure 1 ----

data_socmon <- read_xlsx("data/11_case-studies/socmon/data_socmon.xlsx", sheet = 1) %>% 
  mutate(text_color = "black", 
         year = as.factor(year),
         perception = str_to_sentence(perception),
         perception = factor(perception, c("Very bad", "Bad", "Neither good nor bad", "Good", "Very good", "Not sure")))

ggplot(data = data_socmon, aes(x = year, y = value, fill = perception, label = value, color = text_color)) +
  geom_bar(stat = "identity", width = 0.75,
           position = position_stack(reverse = TRUE), color = "white", linewidth = 0.05) +
  geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
            family = font_choose_graph, size = 3) + 
  coord_flip() +
  facet_wrap(~territory, scales = "free", ncol = 1) +
  scale_color_identity() +
  scale_fill_manual(breaks = c("Very bad", "Bad", "Neither good nor bad", "Good", "Very good", "Not sure"),
                    values = c("#ce6693", "#f8a07e", "white", "#7393C9", "#2C5D96", "grey")) +
  scale_x_discrete(limits = rev) +
  labs(x = NULL, y = "Percentage of respondents", fill = "Frequency") +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.direction = "vertical")

ggsave("figs/03_case-studies/socmon_1.png", width = 10, height = 6, bg = "transparent")
