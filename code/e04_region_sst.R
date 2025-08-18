# 1. Load packages ----

library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)
library(RcppRoll)
library(terra)
library(sf)
library(tidyterra)
library(RColorBrewer)
library(extrafont)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/extract_coeff.R")

theme_set(theme_graph())

# 3. Comparison of warming rates ----

## 3.1 Load and transform data ----

data_warming <- read.csv("data/02_misc/data-warming.csv") %>% 
  select(area, warming_rate, sst_increase) %>% 
  # The number 0.97°C is coming from Forster et al (2024) - Table 5, page 2638
  add_row(area = "Global Ocean", warming_rate = NA, sst_increase = 0.97) %>% 
  mutate(warming_rate = round(warming_rate, 3),
         color = case_when(area == "Global Ocean" ~ "black",
                           area == "Entire Caribbean region" ~ "black",
                           sst_increase > 0 & area != "Global Ocean" ~ "#d35f5fff",
                           sst_increase <= 0 & area != "Global Ocean" ~ palette_first[2]),
         area = if_else(area == "Global Ocean", "**Global Ocean**", area),
         area = if_else(area == "Entire Caribbean region", "**Entire Caribbean region**", area)) %>% 
  arrange(desc(sst_increase)) %>% 
  mutate(area = str_replace_all(area, c("Islands" = "Isl.",
                                                  " and the " = " & ",
                                                  " and " = " & ",
                                                  "United States" = "U.S.",
                                                  "Saint " = "St. "))) %>% 
  filter(area != "Navassa Island")

## 3.2 Make the plot ----

ggplot(data = data_warming, aes(x = sst_increase, y = fct_reorder(area, sst_increase))) +
  geom_bar(stat = "identity", aes(fill = color), width = 0.6, color = "white") +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = "Change in SST (°C)\nbetween 1985 and 2024", y = NULL) +
  theme_graph() +
  theme(axis.text.y = element_markdown()) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.5),
                     breaks = c(0.0, 0.5, 1.0, 1.5),
                     labels = c("0.0", "+0.5", "+1.0", "+1.5"))

## 3.3 Save the plot ----

ggsave("figs/01_part-1/fig-4.png", height = 14, width = 6.1, dpi = fig_resolution)

# 4. SST anomaly ----

## 4.1 Load and transform data ----

load("data/02_misc/data-sst_processed.RData")

data_sst_caribbean <- data_sst %>% 
  filter(area == "Entire Caribbean region") %>% 
  drop_na(sst_anom_mean) %>% 
  mutate(date = as_date(date))

## 4.2 Make the plot ----

plot_anom <- ggplot(data = data_sst_caribbean) +
  geom_ribbon(data = data_sst_caribbean %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = "#d35f5fff", alpha = 0.9) +
  geom_ribbon(data = data_sst_caribbean %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = 0)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

## 4.3 Save the plot ----

ggsave("figs/01_part-1/fig-6a.png", plot = plot_anom, height = 4, width = 5, dpi = fig_resolution)

# 5. SST anomaly with trend ----

## 5.1 Load and transform data ----

load("data/02_misc/data-sst_processed.RData")

data_sst_caribbean <- data_sst %>% 
  filter(area == "Entire Caribbean region") %>% 
  drop_na(sst_anom_mean)

data_sst_caribbean <- data_sst_caribbean %>% 
  mutate(date = as.numeric(as_date(date))) %>% 
  group_by(area) %>% 
  # Extract linear model coefficients
  group_modify(~extract_coeff(data = .x, var_y = "sst_anom_mean", var_x = "date")) %>% 
  ungroup() %>% 
  left_join(data_sst_caribbean, .) %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_anom_mean_linear = slope*date_num+intercept)

## 5.2 Make the plot ----

plot_anom_trend <- ggplot(data = data_sst_caribbean) +
  geom_ribbon(data = data_sst_caribbean %>% mutate(sst_anom_mean = if_else(sst_anom_mean < sst_anom_mean_linear,
                                                                         sst_anom_mean_linear,
                                                                         sst_anom_mean)),
              aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill = "#d35f5fff", alpha = 0.9) +
  geom_ribbon(data = data_sst_caribbean %>% mutate(sst_anom_mean = if_else(sst_anom_mean > sst_anom_mean_linear,
                                                                         sst_anom_mean_linear,
                                                                         sst_anom_mean)),
              aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = sst_anom_mean_linear)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

## 5.3 Save the plot ----

ggsave("figs/01_part-1/fig-6b.png", plot = plot_anom_trend, height = 4, width = 5, dpi = fig_resolution)

# 6. Combine the two SST anomaly plots ----

(plot_anom + labs(title = "A", x = NULL) + theme(plot.title = element_text(face = "bold"))) +
  (plot_anom_trend + labs(title = "B") + theme(plot.title = element_text(face = "bold"))) + plot_layout(ncol = 1)

ggsave("figs/01_part-1/fig-6.png", height = 8, width = 5, dpi = fig_resolution)

# 7. Comparison of SST distribution ----

## 7.1 Transform data ----

data_sst <- data_sst %>% 
  filter(!(area %in% c("Entire Caribbean region", "Navassa Island"))) %>% 
  group_by(area) %>% 
  summarise(mean = mean(sst)) %>% 
  ungroup() %>% 
  left_join(., data_sst) %>% 
  mutate(area = str_replace_all(area, c("Islands" = "Isl.",
                                                  " and the " = " & ",
                                                  " and " = " & ",
                                                  "United States" = "U.S.",
                                                  "Saint " = "St. ")))

## 7.2 Make the plot ----

ggplot(data = data_sst, aes(x = sst, y = fct_reorder(area, mean))) +
  geom_violin(draw_quantiles = c(0.5), fill = palette_first[3], color = palette_first[4], alpha = 0.5) +
  labs(x = "SST (°C)", y = NULL) +
  theme_graph() +
  coord_cartesian(clip = "off")

## 7.3 Save the plot ----

ggsave("figs/06_additional/01_misc/sst_distribution.png", height = 12, width = 6, dpi = fig_resolution)

# 8. Map of mean SST anomaly per year ----

## 8.1 Load data ----

data_land <- read_sf("data/01_maps/02_clean/05_princeton/land.shp")

data_eez <- read_sf("data/01_maps/02_clean/03_eez/caribbean_area.shp")

## 8.2 List of files ----

data_files <- tibble(path = list.files("data/05_crw_year/", full.names = TRUE)) %>% 
  filter(str_detect(path, "ssta_mean") == TRUE) %>% 
  mutate(year = as.numeric(str_sub(path, -7, -4)),
         group = rep(1:50, each = 8, length.out = nrow(.))) # 8 is the number of subplots (i.e. years) per plot

## 8.3 Create the function to make the plot for each year ----

map_ssta_year <- function(year_i, data_files_i){
  
  # 1. Load data
  
  raster <- rast(data_files_i %>% filter(year == year_i) %>% select(path) %>% pull)$sea_surface_temperature_anomaly
  
  # 2. Make the plot
  
  ggplot() +
    geom_spatraster(data = raster) +
    geom_sf(data = data_eez, fill = NA, linewidth = 0.15) +
    geom_sf(data = data_land, fill = "#363737", col = "grey", linewidth = 0.15) +
    scale_fill_gradientn(colours = c(rev(palette_first), "white", palette_second),
                         limits = c(-5, 5),
                         name = "Yearly average SST anomaly (°C)",
                         guide = guide_colourbar(direction = "horizontal", 
                                                 title.position = "top", 
                                                 title.hjust = 0.5, 
                                                 ticks.colour = "black",
                                                 frame.colour = "black")) +
    coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35)) +
    labs(title = year_i) +
    theme(text = element_text(family = "Open Sans"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(family = "Open Sans"),
          plot.title = element_text(family = "Open Sans", hjust = 0.5),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.key.width = unit(2.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.position = "bottom")
  
}

## 8.4 Create the function to make the plot for each group ----

map_ssta_plot <- function(group_i){
  
  # 1. Filter the data_files
  
  data_files_i <- data_files %>% 
    filter(group == group_i)
  
  # 2. Create all the plots
  
  plots <- map(c(data_files_i$year), ~map_ssta_year(data_files_i = data_files_i, year_i = .))
  
  # 3. Combine plots
  
  combined_plots <- wrap_plots(plots) + 
    plot_layout(guides = "collect", ncol = 2) & 
    theme(legend.position = "bottom")
  
  # 4. Save the plot
  
  ggsave(filename = paste0("figs/06_additional/01_misc/map_sst-anom_",
                           min(data_files_i$year), "-", max(data_files_i$year), ".png"),
         height = 14, width = 9, combined_plots, dpi = 600)
  
}

## 8.5 Map over the function ----

map(unique(data_files$group), ~map_ssta_plot(group_i = .))
