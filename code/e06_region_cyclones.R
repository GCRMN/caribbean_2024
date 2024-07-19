# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2. Source functions ----

source("code/function/plot_region.R")
source("code/function/data_descriptors.R")
source("code/function/plot_region.R")
source("code/function/theme_graph.R")

# 3. Load cyclones ----

load("data/07_cyclones/01_cyclones_lines.RData")
load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  filter(saffir > 0) %>% 
  mutate(saffir = as.factor(saffir)) %>% 
  right_join(data_ts_lines, .)

# 4. Make the plot ----

plot <- plot_region(scale = TRUE) +
  geom_sf(data = data_cyclones %>% arrange(saffir), aes(color = saffir),
          alpha = 0.75, linewidth = 0.5, show.legend = "line") +
  scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                     labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                     values = c(palette_second[2:5], "black"),
                     name = "Saffir-Simpson category",
                     drop = FALSE) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(linewidth = 1))) +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

ggsave(filename = "figs/01_part-1/fig-6.png", plot = plot,
       width = 7.5, height = 5.75, dpi = fig_resolution)

# 5. Comparison of cyclones occurrence ----

## 5.1 Transform data ----

data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_eez.shp")

data_cyclones <- data_cyclones %>% 
  st_drop_geometry() %>% 
  group_by(territory, saffir) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(territory) %>% 
  mutate(n_tot = sum(n)) %>% 
  ungroup() %>% 
  bind_rows(., tibble(territory = setdiff(data_eez$territory, data_cyclones$territory),
                      n = rep(0, length(setdiff(data_eez$territory, data_cyclones$territory))),
                      n_tot = n)) %>% 
  filter(!(territory %in% c("Quitasueño Bank", "Serrana Bank", "Navassa Island"))) %>% 
  mutate(territory = str_replace_all(territory, c("Islands" = "Isl.",
                                                  " and the " = " & ",
                                                  " and " = " & ",
                                                  "Saint" = "St.")))

## 5.2 Make the plot ----

ggplot(data = data_cyclones, aes(x = n, y = fct_reorder(territory, n_tot), fill = saffir)) +
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
  labs(x = "Number of cyclones", y = NULL)

## 5.3 Save the plot ----

ggsave(filename = "figs/01_part-1/fig-7.png", width = 6, height = 12, dpi = fig_resolution)

# 6. Key numbers ----

load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1)

## 6.1 Total number of cyclones ----

nrow(data_cyclones)

## 6.2 Max number of cyclones ----

data_cyclones <- data_cyclones %>% 
  select(ts_id, territory) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n == max(n))

## 6.3 Highest windspeed ----

load("data/07_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  arrange(desc(windspeed))
