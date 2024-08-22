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
  filter(!(territory %in% c("Entire Caribbean region"))) %>% 
  select(territory, warming_rate, sst_increase) %>% 
  # The number 0.88°C is coming from Technical summary IPCC, 2021 (TS.2.4, The Ocean, page 74)
  add_row(territory = "Global Ocean", warming_rate = (0.88/(2020-1900))*(2022-1980), sst_increase = 0.88) %>% 
  mutate(warming_rate = round(warming_rate, 3),
         color = case_when(territory == "Global Ocean" ~ "black",
                           sst_increase > 0 & territory != "Global Ocean" ~ "#ce6693",
                           sst_increase <= 0 & territory != "Global Ocean" ~ palette_first[2]),
         territory = if_else(territory == "Global Ocean", "**Global Ocean**", territory)) %>% 
  arrange(desc(sst_increase)) %>% 
  mutate(territory = str_replace_all(territory, c("Islands" = "Isl.",
                                                  " and the " = " & ",
                                                  " and " = " & ",
                                                  "United States" = "U.S.",
                                                  "Saint " = "St. ")))

## 3.2 Make the plot ----

ggplot(data = data_warming, aes(x = sst_increase, y = fct_reorder(territory, sst_increase))) +
  geom_bar(stat = "identity", aes(fill = color), width = 0.6, color = "white") +
  scale_fill_identity() +
  scale_color_identity() +
  geom_vline(xintercept = 0) +
  labs(x = "Change in SST (°C)\nbetween 1985 and 2024", y = NULL) +
  theme_graph() +
  theme(axis.text.y = element_markdown()) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 1.5))

## 3.3 Save the plot ----

ggsave("figs/01_part-1/fig-4.png", height = 12, width = 6, dpi = fig_resolution)

# 4. SST anomaly ----

## 4.1 Load and transform data ----

load("data/02_misc/data-sst_processed.RData")

data_sst_caribbean <- data_sst %>% 
  filter(territory == "Entire Caribbean region") %>% 
  drop_na(sst_anom_mean) %>% 
  mutate(date = as_date(date))

## 4.2 Make the plot ----

plot_anom <- ggplot(data = data_sst_caribbean) +
  geom_ribbon(data = data_sst_caribbean %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
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
  filter(territory == "Entire Caribbean region") %>% 
  drop_na(sst_anom_mean)

data_sst_caribbean <- data_sst_caribbean %>% 
  mutate(date = as.numeric(as_date(date))) %>% 
  group_by(territory) %>% 
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
              aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
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
  filter(!(territory %in% c("Entire Caribbean region"))) %>% 
  group_by(territory) %>% 
  summarise(mean = mean(sst)) %>% 
  ungroup() %>% 
  left_join(., data_sst)

## 7.2 Make the plot ----

ggplot(data = data_sst, aes(x = sst, y = fct_reorder(territory, mean))) +
  geom_violin(draw_quantiles = c(0.5), fill = "#446CB3", alpha = 0.5) +
  labs(x = "SST (°C)", y = NULL) +
  theme_graph() +
  coord_cartesian(clip = "off")

## 7.3 Save the plot ----

ggsave("figs/05_supp-mat/sst-distribution_territories.png", height = 12, width = 8, dpi = fig_resolution)
