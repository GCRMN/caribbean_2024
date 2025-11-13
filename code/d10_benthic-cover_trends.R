# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(ggrepel)
library(scales)
library(zoo)
library(Kendall)
library(sf)
sf_use_s2(FALSE)
library(cowplot) # For the function draw_image()
library(ggspatial) # For annotation_scale function

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map.R")
source("code/function/add_colors.R")
source("code/function/combine_model_data.R")
source("code/function/extract_mannkendall.R")
source("code/function/plot_vimp.R")
source("code/function/plot_pdp.R")
source("code/function/plot_residuals.R")
source("code/function/plot_pred_obs.R")
source("code/function/plot_prediction_map.R")
source("code/function/plot_trends_chapters.R")
source("code/function/model_text.R")
source("code/function/limits_region.R")

theme_set(theme_graph())

# 3. Data preparation ----

## 3.1 Combine model results ----

model_results <- combine_model_data(save_results = FALSE)

model_results$result_trends <- model_results$result_trends %>% 
  filter(year >= 1980 & year <= 2024) %>% 
  drop_na(area)

## 3.2 Confidence intervals ----

raw_trends <- model_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, region, area, year, color, text_title) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.025),
            upper_ci_95 = quantile(cover, 0.975)) %>% 
  ungroup() %>% 
  # Replace negative values by 0
  mutate(across(c(mean, lower_ci_95, upper_ci_95), ~ifelse(.x < 0, 0, .x)))

## 3.3 Long-term average ----

long_term_average <- raw_trends %>% 
  group_by(category, area) %>% 
  summarise(across(c(mean, lower_ci_95, upper_ci_95), ~mean(.x, na.rm = TRUE))) %>% 
  ungroup()

## 3.4 Long-term trend ----

long_term_trend <- raw_trends %>% 
  group_by(category, area) %>% 
  group_modify(~extract_mannkendall(data = .x, var_y = "mean")) %>% 
  ungroup()

## 3.5 Add "obs_data" variable ----

load("data/09_model-data/data_benthic_prepared.RData")

data_benthic_obs <- data_benthic %>% 
  select(year, area, category) %>% 
  distinct() %>% 
  mutate(region = "Caribbean",
         data_obs = 1)

data_benthic_obs <- data_benthic %>% 
  select(year, category) %>% 
  distinct() %>% 
  mutate(region = "Caribbean",
         area = "All",
         data_obs = 1) %>% 
  bind_rows(., data_benthic_obs)

raw_trends <- left_join(raw_trends, data_benthic_obs) %>% 
  mutate(data_obs = replace_na(data_obs, 0))

## 3.6 Combine into a list ----

data_trends <- lst(raw_trends, long_term_average, long_term_trend)

rm(raw_trends, long_term_average,
   long_term_trend, data_benthic_obs)

# 4. Model evaluation ----

## 4.1 Hyper-parameters ----

model_results$model_hyperparams

## 4.2 Performance (RMSE and R²) ----

### 4.2.1 Entire Caribbean region ----

data_rmse_rsq <- model_results$model_performance %>% 
  group_by(category) %>% 
  summarise(across(c("rmse", "rsq"), ~mean(.x))) %>% 
  ungroup() %>% 
  mutate(area = "All")
  
### 4.2.2 Per area ----

perf_area <- function(category_i, area_i){
  
  rmse <- model_results$result_pred_obs %>% 
    filter(area == area_i, category == category_i) %>% 
    summarise(yardstick::rmse(data = ., truth = y, estimate = yhat)) %>% 
    select(.estimate) %>% 
    pull()
  
  rsq <- model_results$result_pred_obs %>% 
    filter(area == area_i, category == category_i) %>% 
    summarise(yardstick::rsq(data = ., truth = y, estimate = yhat)) %>% 
    select(.estimate) %>% 
    pull()
  
  result_i <- tibble(category = category_i,
                     area = area_i,
                     rmse = rmse,
                     rsq = rsq)

  return(result_i)
  
}

perf_levels <- expand.grid(category = unique(model_results$result_pred_obs$category),
            area = unique(model_results$result_pred_obs$area))

data_rmse_rsq <- map2(.x = perf_levels$category, .y = perf_levels$area, ~perf_area(category_i = .x, area_i = .y)) %>% 
  list_rbind() %>% 
  bind_rows(., data_rmse_rsq) %>% 
  mutate(across(c("rmse", "rsq"), ~round(.x, 2)),
         across(c("rmse", "rsq"), ~ifelse(is.nan(.x) == TRUE, NA, .x)))
  
rm(perf_levels, perf_area)

data_rmse_rsqU <- data_rmse_rsq %>% 
  filter(category %in% c("Macroalgae", "Hard coral")) %>%
  filter(!(area %in% c("Navassa Island", "All"))) %>%
  drop_na(area) %>% 
  pivot_wider(names_from = "category", values_from = c(rmse, rsq), names_sep = "_") %>% 
  arrange(area) %>% 
  select(area, "rmse_Hard coral", "rsq_Hard coral", "rmse_Macroalgae", "rsq_Macroalgae") %>% 
  openxlsx::write.xlsx("figs/05_supp-mat/supp_tbl_2.xlsx")

## 4.4 Residuals ----

### 4.4.1 Distribution of residuals ----

plot_i <- model_results$result_pred_obs %>%
  filter(category %in% c("Hard coral", "Algae", "Macroalgae", "Turf algae",
                         "Coralline algae", "Other fauna", "Acropora",
                         "Orbicella", "Porites")) %>% 
  mutate(residual = yhat - y) %>% 
  ggplot(data = ., aes(x = residual, fill = color)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))*100),
                 alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_fill_identity() +
  facet_wrap(~category, scales = "free") +
  lims(x = c(-100, 100)) +
  labs(x = "Residual (ŷ - y)", y = "Percentage") +
  theme(strip.text = element_markdown(hjust = 0, face = "bold"),
        strip.background = element_blank(),
        axis.text = element_text(size = 8))

ggsave(plot_i, filename = "figs/06_additional/03_model-evaluation/residuals_distribution.png",
       dpi = fig_resolution, height = 6, width = 8)

### 4.4.2 Pred vs Obs ----

plot_i <- model_results$result_pred_obs %>%
  filter(category %in% c("Hard coral", "Algae", "Macroalgae", "Turf algae",
                         "Coralline algae", "Other fauna", "Acropora",
                         "Orbicella", "Porites")) %>%
  ggplot(data = ., aes(x = y, y = yhat, color = color)) +
  geom_abline(slope = 1, linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
  scale_color_identity() +
  facet_wrap(~category) +
  labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
  theme(strip.text = element_markdown(hjust = 0, face = "bold"),
        strip.background = element_blank(),
        axis.text = element_text(size = 8))

ggsave(plot_i, filename = "figs/06_additional/03_model-evaluation/residuals_pred-obs.png",
       dpi = fig_resolution, height = 6, width = 8)

### 4.4.3 Mean residuals per year ----

plot_i <- model_results$result_pred_obs %>%
  filter(category %in% c("Hard coral", "Algae", "Macroalgae", "Turf algae",
                         "Coralline algae", "Other fauna", "Acropora",
                         "Orbicella", "Porites")) %>% 
  mutate(residual = yhat - y) %>% 
  group_by(category, year, color) %>% 
  summarise(mean = mean(residual)) %>% 
  ungroup() %>% 
  ggplot(data = ., aes(x = year, y = mean, color = color)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_color_identity() +
  facet_wrap(~category, scales = "free") +
  labs(y = "Residual (ŷ - y)", x = "Year") +
  theme(strip.text = element_markdown(hjust = 0, face = "bold"),
        strip.background = element_blank(),
        axis.text = element_text(size = 8))

ggsave(plot_i, filename = "figs/06_additional/03_model-evaluation/residuals_year.png",
       dpi = fig_resolution, height = 6, width = 8)

## 4.5 VIMP ----

### 4.5.1 Transform data ----

data_imp_summary <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_")) %>% 
  group_by(predictor, category, color) %>% 
  summarise(mean = mean(importance),
            lower_ci_95 = quantile(importance, 0.025),
            upper_ci_95 = quantile(importance, 0.975)) %>% 
  ungroup() %>% 
  group_by(category, color) %>% 
  arrange(desc(mean)) %>% 
  slice_head(n = 25) %>% 
  ungroup()

data_imp_raw <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_")) %>% 
  left_join(data_imp_summary, .)

### 4.5.2 Map over the function ----

map(unique(data_imp_summary$category), ~plot_vimp(category_i = .))

rm(data_imp_raw)

# 5. Temporal trends ----

## 5.1 Load and transform obs data ----

load("data/09_model-data/data_benthic_prepared.RData")

data_benthic_raw <- data_benthic %>% 
  add_colors()

data_benthic <- data_benthic_raw %>% 
  bind_rows(., data_benthic_raw %>% 
              mutate(area = "All")) %>% 
  group_by(year, area, category, color, text_title) %>% 
  summarise(mean = mean(measurementValue)) %>% 
  ungroup() %>% 
  bind_rows(., data_benthic_raw %>% 
              group_by(year, category, color, text_title) %>% 
              summarise(mean = mean(measurementValue)) %>% 
              ungroup() %>% 
              mutate(area = "Caribbean")) %>% 
  complete(year, category, nesting(area), fill = list(mean = NA)) %>% 
  select(-color, -text_title) %>% 
  left_join(., data_benthic_raw %>% 
              select(category, text_title, color) %>% 
              distinct()) %>% 
  drop_na(area)

## 5.2 Figure 12 (Part 1) ----

data_trends_i <- data_trends$raw_trends %>% 
  filter(area == "All") %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae", "Other fauna"))

data_raw_i <- data_benthic %>% 
  filter(area == "All") %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae", "Other fauna"))

plot_trends <- ggplot() +
    geom_point(data = data_raw_i,
                    aes(x = year, y = mean, color = "#b2bec3"), size = 1) +
    geom_ribbon(data = data_trends_i,
                aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95,
                    fill = color), alpha = 0.5, show.legend = FALSE) +
    geom_line(data = data_trends_i,
              aes(x = year, y = mean, color = color),
              linewidth = 1, show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(~text_title, scales = "free", ncol = 3) +
  theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines")) +
  labs(x = "Year", y = "Benthic cover (%)") +
  lims(x = c(1980, 2024), y = c(0, 60))

ggsave(plot = plot_trends, filename = "figs/01_part-1/fig-12_raw.png", width = 12, height = 8)

## 5.3 Figure 13 (Part 1) ----

data_trends_i <- data_trends$raw_trends %>% 
  filter(area == "All") %>% 
  filter(category %in% c("Acropora", "Orbicella", "Porites"))

data_raw_i <- data_benthic %>% 
  filter(area == "All") %>% 
  filter(category %in% c("Acropora", "Orbicella", "Porites"))

plot_trends <- ggplot() +
  geom_point(data = data_raw_i,
             aes(x = year, y = mean, color = "#b2bec3"), size = 1) +
  geom_ribbon(data = data_trends_i,
              aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95,
                  fill = color), alpha = 0.5, show.legend = FALSE) +
  geom_line(data = data_trends_i,
            aes(x = year, y = mean, color = color),
            linewidth = 1, show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(~text_title, scales = "fixed", ncol = 3) +
  theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines")) +
  labs(x = "Year", y = "Benthic cover (%)") +
  lims(x = c(1980, 2024), y = c(0, NA))

ggsave(plot = plot_trends, filename = "figs/01_part-1/fig-13_raw.png", width = 12, height = 4.5)

rm(data_trends_i, data_raw_i)

## 5.4 Figure 5 (Part 2) -----

data_benthic %>% 
  filter(!(area %in% c("All", "Navassa Island", "Caribbean"))) %>% 
  select(area) %>% 
  distinct() %>% 
  pull() %>% 
  map(.,
      ~plot_trends_chapters(area_i = .x,
                            icons = TRUE,
                            scales = "fixed",
                            raw_data = TRUE,
                            modelled_data = TRUE))

## 5.5 Figure 5b (Part 2) -----

data_benthic %>% 
  filter(!(area %in% c("All", "Navassa Island", "Caribbean"))) %>% 
  select(area) %>% 
  distinct() %>% 
  pull() %>% 
  map(.,
      ~plot_trends_chapters(area_i = .x,
                            icons = TRUE,
                            scales = "fixed",
                            raw_data = TRUE,
                            reefcheck = TRUE,
                            modelled_data = FALSE))

## 5.5 Figure 5c (Part 2) -----

data_benthic %>% 
  filter(!(area %in% c("All", "Navassa Island", "Caribbean"))) %>% 
  select(area) %>% 
  distinct() %>% 
  pull() %>% 
  map(.,
      ~plot_trends_chapters(area_i = .x,
                            icons = TRUE,
                            scales = "fixed",
                            raw_data = TRUE,
                            reefcheck = FALSE,
                            modelled_data = FALSE))

# Raw data (for writing)

if(FALSE){
  
  A <- data_trends$raw_trends %>%
    filter(area == "All" & category == "Macroalgae") %>%
    select(-text_title, -color)

  A %>% 
    filter(year %in% c(2009:2024)) %>% 
    summarise(across(c("mean", "lower_ci_95", "upper_ci_95"), ~mean(.x)))
  
  data_trends$long_term_average %>% 
    filter(area == "All")
  
  data_trends$long_term_trend %>% 
    filter(area == "All")
    
}

# 6. Generate text to describe models ----

map(unique(data_trends$raw_trends$category), ~model_text(category_i = .x))

# 7. Figures for the Executive Summary ----

## 7.1 Hard coral ----

plot_i <- data_trends$raw_trends %>% 
  filter(category == "Hard coral" & area == "All") %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = "#c44d56"), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, color = "#c44d56"), linewidth = 1) +
  annotate("rect", xmin = 1970, xmax = 1979.5, ymin = 29, ymax = 38, fill = "#c44d56", alpha = 0.35) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1970, NA)) +
  scale_y_continuous(limits = c(0, 60))

plot_i +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/00_misc/exe-summ_en_coral_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

plot_i +
  labs(x = "Année", y = "Couverture benthique (%)")

ggsave("figs/00_misc/exe-summ_fr_coral_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

plot_i +
  labs(x = "Año", y = "Cobertura bentónica (%)")

ggsave("figs/00_misc/exe-summ_sp_coral_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

## 7.2 Macroalgae ----

plot_i <- data_trends$raw_trends %>% 
  filter(category == "Macroalgae" & area == "All") %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = "#03a678"), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, color = "#03a678"), linewidth = 1) +
  annotate("rect", xmin = 1970, xmax = 1979.5, ymin = 4, ymax = 13, fill = "#03a678", alpha = 0.35) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1970, NA)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Year", y = "Benthic cover (%)")

plot_i +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/00_misc/exe-summ_en_macroalgae_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

plot_i +
  labs(x = "Année", y = "Couverture benthique (%)")

ggsave("figs/00_misc/exe-summ_fr_macroalgae_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

plot_i +
  labs(x = "Año", y = "Cobertura bentónica (%)")

ggsave("figs/00_misc/exe-summ_sp_macroalgae_raw.png", height = 5.3, width = 9.2, dpi = fig_resolution)

# 8. Figures for the poster ----

## 8.1 Hard coral ----

plot_a <- data_trends$raw_trends %>% 
  filter(category == "Hard coral" & area == "All") %>% 
  filter(year >= 1984) %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = "#F56b12"), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, color = "#F56b12"), linewidth = 1) +
  annotate("rect", xmin = 1972, xmax = 1983, ymin = 29, ymax = 38, fill = "#F56b12", alpha = 0.2) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1970, NA)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Year", y = "Benthic cover (%)") +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.title = element_text(color = "white"),
        axis.title.x = element_text(size = 36),
        axis.title.y = element_text(size = 36),
        axis.text = element_text(color = "white", size = 30),
        axis.line = element_line(color = "white"),
        axis.line.x = element_line(linewidth = 0.6),
        axis.ticks.x = element_line(linewidth = 0.6),
        axis.ticks.y = element_line(linewidth = 0.6),
        axis.ticks = element_line(color = "white"))

## 8.2 Macroalgae ----

plot_b <- data_trends$raw_trends %>% 
  filter(category == "Macroalgae" & area == "All") %>% 
  filter(year >= 1984) %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = "#24bf25"), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, color = "#24bf25"), linewidth = 1) +
  annotate("rect", xmin = 1972, xmax = 1983, ymin = 4, ymax = 13, fill = "#24bf25", alpha = 0.2) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1970, NA)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Year", y = "Benthic cover (%)") +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank(),
        axis.title = element_text(color = "white"),
        axis.title.x = element_text(size = 36),
        axis.title.y = element_text(size = 36),
        axis.text = element_text(color = "white", size = 30),
        axis.line = element_line(color = "white"),
        axis.line.x = element_line(linewidth = 0.6),
        axis.ticks.x = element_line(linewidth = 0.6),
        axis.ticks.y = element_line(linewidth = 0.6),
        axis.ticks = element_line(color = "white"))

## 8.3 Combine and export plots ----

plot_a + plot_b + 
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  ))

ggsave("figs/06_additional/01_misc/poster_fig-2.png", dpi = 600, width = 11, height = 5, bg = "transparent")
ggsave("figs/06_additional/01_misc/poster_fig-2.svg", dpi = 600, width = 11, height = 5, bg = "transparent")

# 9. Additional figures ----

## 9.1 Hard coral vs algae ----

### 9.1.1 Transform data ----

data_hc_algae <- data_trends$raw_trends %>% 
  filter(area == "All" & category %in% c("Hard coral", "Algae")) %>% 
  select(year, category, mean) %>% 
  pivot_wider(names_from = "category", values_from = "mean") %>% 
  mutate(ratio = `Hard coral`/`Algae`)

### 9.1.2 Create labels ----

data_labels <- tibble(type = c(1, 1, 2),
                      x = c(30, 10, 50),
                      y = c(10, 30, 50),
                      text = c("**More <span style='color:#16a085'>algae</span>**<br>than <span style='color:#c44d56'>hard corals</span>",
                               "**More <span style='color:#c44d56'>hard corals</span>**<br>than <span style='color:#16a085'>algae</span>",
                               "As much <span style='color:#c44d56'>hard corals</span> than <span style='color:#16a085'>algae</span>"))

### 9.1.3 Make the plot ----

ggplot(data = data_hc_algae, aes(x = Algae, y = `Hard coral`, label = year)) +
  geom_line() +
  geom_text_repel(data = data_hc_algae %>% filter(year %in% c(1985, 2000, 2010, 2023)),
                  aes(x = Algae, y = `Hard coral`, label = year), force = 40,
                  family = font_choose_graph, seed = 27, min.segment.length = unit(10, "cm")) + 
  geom_point(data = data_hc_algae, aes(x = Algae, y = `Hard coral`, fill = ratio),
             size = 2, shape = 21, show.legend = FALSE, color = "black") +
  geom_point(data = data_hc_algae %>% filter(year %in% c(1985, 2000, 2010, 2023)),
             aes(x = Algae, y = `Hard coral`, fill = ratio), size = 4.5, shape = 21,
             color = "black", show.legend = FALSE) +
  geom_abline(slope = 1, linetype = "dashed") +
  scale_fill_gradient2(high = "#c44d56", low = "#16a085", midpoint = 1) +
  labs(x = "Algae cover (%)", y = "Hard coral cover (%)") +
  lims(x = c(0, 60), y = c(0, 60)) +
  theme(axis.line.y = element_line(linewidth = 0.4),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black")) +
  geom_richtext(data = data_labels %>% filter(type == 1), aes(x = x, y = y, label = text),
                label.color = "transparent", fill = "transparent", size = 3) +
  geom_richtext(data = data_labels %>% filter(type == 2), aes(x = x, y = y, label = text),
                label.color = "transparent", fill = "#efeff0", size = 3, angle = 45)

ggsave("figs/06_additional/01_misc/hard-coral-vs-algae.png", width = 6, height = 6, dpi = fig_resolution)

## 9.2 Stacked benthic cover ----

### 9.2.1 Transform data ----

data_cover <- data_trends$raw_trends %>% 
  filter(area == "All" & category %in% c("Hard coral", "Algae", "Other fauna"))

data_cover <- data_cover %>% 
  group_by(year) %>% 
  summarise(mean = sum(mean)) %>% 
  ungroup() %>% 
  # Generate the others category (difference between 100 and sum of all categories)
  mutate(mean = 100 - mean, 
         category = "Others",
         color = "lightgrey") %>% 
  bind_rows(data_cover, .)

data_labels <- tibble(x = c(1995, 1995, 1995, 1995),
                      y = c(82, 52.5, 36, 14),
                      category = c("Algae", "Hard coral", "Other fauna", "Others"),
                      color = c("white", "white", "white", "black"))

### 9.2.2 Make the plot ----

ggplot(data = data_cover, aes(x = year, y = mean, fill = category)) +
  geom_area(show.legend = FALSE, color = "white", outline.type = "full", linewidth = 0.25) +
  scale_fill_manual(values = unique(data_cover$color)) +
  theme_graph() +
  geom_text(data = data_labels, aes(x = x, y = y, label = category, color = color),
            size = 4.5, family = font_choose_graph) +
  scale_color_identity() +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/06_additional/01_misc/stacked-benthic-cover.png", width = 7, height = 5, dpi = fig_resolution)

## 9.3 Comparison previous trends ----

load("C:/Users/jerem/Desktop/Recherche/03_projects/2025-08-25_time-series/time_series/data/data_trends_litterature.RData")

data_trends_litterature <- data_trends$raw_trends %>%
  filter(area == "All") %>% 
  rename(lower_ci = lower_ci_95, higher_ci = upper_ci_95) %>% 
  select(category, region, year, mean, higher_ci, lower_ci) %>%
  mutate(source = "GCRMN Caribbean 2025") %>% 
  bind_rows(., data_trends_litterature)

data_trends_litterature %>% 
  filter(region == "Caribbean" & is.na(subregion) & category == "Hard coral") %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci, ymax = higher_ci), fill = "lightgrey") +
  geom_point(aes(x = year, y = mean)) +
  geom_line(aes(x = year, y = mean)) +
  facet_wrap(~source, nrow = 1) +
  lims(y = c(0, NA)) +
  theme_graph() +
  theme(strip.background = element_blank(),
        panel.spacing = unit(3, "lines")) +
  labs(x = "Year", y = "Hard coral cover (%)")

ggsave("figs/06_additional/01_misc/trends_litterature.png", width = 15, height = 5)

## 9.4 Confidence in estimated trends ----

### 9.4.1 Load data ----

data_confidence <- readxl::read_xlsx("data/02_misc/confidence-levels_trends.xlsx")

data_land <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_eez <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_land_cropped <- st_intersection(data_land, data_crop)

data_eez <- st_difference(data_eez, st_union(data_land_cropped)) %>% 
  left_join(., data_confidence) %>% 
  drop_na(confidence)

data_land_boundaries <- st_read("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_boundary_lines_land/ne_10m_admin_0_boundary_lines_land.shp")

### 9.4.2 Make the map ----

plot <- ggplot() +
  geom_sf(data = data_eez, aes(fill = confidence), color = "white", linewidth = 0.15,
          show.legend = FALSE, alpha = 0.9) +
  scale_fill_manual(values = c("#7393C9", "#f8a07e", "#ce6693", "#34495e"),
                    breaks = c("high", "medium", "low", "excluded")) +
  geom_sf(data = data_land, color = "white", fill = "#d9d9d9", linewidth = 0.05) +
  geom_sf(data = data_land_boundaries, color = "#979796", fill = NA, linewidth = 0.15) +
  limits_region() +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                   text_cex = 0.6, style = "bar", line_width = 1,  height = unit(0.04, "cm"), line_col = "black",
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black")) +
  theme_map() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black"),
        legend.position = "inside",
        legend.direction = "vertical",
        axis.ticks.y = element_line(colour = "grey20"),
        legend.background = element_rect(color = "black", linewidth = 0.1, fill = "#fbfbfb"),
        legend.title = element_text(size = 7, hjust = 0),
        legend.text = element_text(size = 6, margin = margin(t = 0)),
        legend.key.size = unit(0.4, "cm"),
        legend.position.inside = c(0.1, 0.21))

ggsave(filename = "figs/02_part-2/fig-1_raw.png", plot = plot,
       width = 7.25, height = 4.75, dpi = fig_resolution)
