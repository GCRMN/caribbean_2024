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

# 6. Red Hind ----

data_red <- read_xlsx("data/11_case-studies/Red hind data-Nemeth-GCRMN report.xlsx", skip = 1) %>% 
  add_column(nb_points = c(323, 1620, 495, 238, 1176, 2756, 1238, NA, 1203))

data_labels <- tibble(x = c(1.7, 4.5, 7.5),
                      y = rep(10, 3),
                      labels = c("pre-closure", "seasonal closure", "permanent closure"))

ggplot() +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  geom_path(data = data_red %>% drop_na(`Total length (cm)`),
            aes(x = Year, y = `Total length (cm)`, group = 1), color = "#42b9bc") +
  geom_linerange(data = data_red, aes(x = Year, y = `Total length (cm)`,
                                      ymin = `Total length (cm)` - `St. Dev.`,
                                      ymax = `Total length (cm)` + `St. Dev.`),
                  color = "#42b9bc") +
  geom_point(data = data_red, aes(x = Year,  y = `Total length (cm)`),
             fill = "#42b9bc", color = "white", shape = 21, size = 4) +
  geom_label(data = data_labels, aes(x = x, y = y, label = labels),
             fill = "#42b9bc", color = "white", family = font_choose_graph,
             label.padding = unit(0.5, "lines"), fontface = "bold") +
  geom_text(data = data_red,
            aes(x = Year, y = `Total length (cm)` + `St. Dev.`, label = format(nb_points, big.mark = ",")),
            size = 3.5, angle = 90, hjust = -0.25, family = font_choose_graph) +
  lims(y = c(0, 50)) +
  theme_graph() +
  labs(x = "Years") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))

ggsave("figs/03_case-studies/red_hind.png", width = 10, height = 5, bg = "transparent")

# 7. Nassau Grouper ----

data <- read_xlsx("data/11_case-studies/Nassau grouper data-Nemeth-GCRMN report.xlsx",
                  skip = 2, col_types = "numeric")

plot_a <- ggplot(data = data, aes(x = `Year`, y = `Density (no./100m2)`)) +
  geom_line(color = "#42b9bc") +
  geom_point(size = 4, color = "white", fill = "#42b9bc", shape = 21) +
  labs(title = "A", y = bquote("Density (no.100 "~m^-2*")"), x = NULL) +
  theme_graph() +
  theme(plot.title = element_text(face = "bold"))

plot_b <- ggplot(data = data, aes(x = `Year`, y = `Maximum number`)) +
  geom_bar(stat = "identity", fill = "#42b9bc", width = 0.7) +
  labs(title = "B", y = "Maximum abundance") +
  theme_graph() +
  theme(plot.title = element_text(face = "bold"))

plot_a + plot_b + plot_layout(ncol = 1)

ggsave("figs/03_case-studies/nassau_grouper.png", width = 10, height = 8, bg = "transparent")
