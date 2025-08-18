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

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/add_colors.R")
source("code/function/combine_model_data.R")
source("code/function/extract_mannkendall.R")
source("code/function/plot_vimp.R")
source("code/function/plot_pdp.R")
source("code/function/plot_residuals.R")
source("code/function/plot_pred_obs.R")
source("code/function/plot_prediction_map.R")
source("code/function/plot_trends.R")

theme_set(theme_graph())

# 3. Data preparation ----

## 3.1 Combine model results ----

model_results <- combine_model_data(model = "xgb")

model_results$result_trends <- model_results$result_trends %>% 
  filter(year >= 1980 & year <= 2023) %>% 
  drop_na(area)

## 3.2 Confidence intervals ----

raw_trends <- model_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, region, area, year, color, text_title) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.05),
            lower_ci_80 = quantile(cover, 0.20),
            upper_ci_95 = quantile(cover, 0.95),
            upper_ci_80 = quantile(cover, 0.80)) %>% 
  ungroup() %>% 
  # Replace negative values by 0
  mutate(across(c(mean, lower_ci_95, lower_ci_80, upper_ci_95, upper_ci_80), ~ifelse(.x < 0, 0, .x)))

## 3.3 Long-term average ----

long_term_average <- raw_trends %>% 
  group_by(category, area) %>% 
  summarise(across(c(mean, lower_ci_80, upper_ci_80), ~mean(.x, na.rm = TRUE))) %>% 
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

## 3.6 Smooth moving average ----

smoothed_trends <- raw_trends %>% 
  group_by(category, region, area, color, text_title) %>% 
  # Two years moving average
  mutate(across(c("mean", "lower_ci_95", "lower_ci_80", "upper_ci_95", "upper_ci_80"),
                ~rollmean(.x, k = 2, fill = NA, align = "center"))) %>% 
  ungroup() %>% 
  # Add first year data
  filter(year != 1980) %>% 
  bind_rows(., raw_trends %>% 
              filter(year == 1980)) %>% 
  arrange(category, region, area, year)

## 3.7 Combine into a list ----

data_trends <- lst(smoothed_trends, raw_trends, long_term_average, long_term_trend)

rm(smoothed_trends, raw_trends, long_term_average,
   long_term_trend, data_benthic_obs)

# 4. Model evaluation ----

## 4.1 Hyper-parameters ----

model_results$model_description %>% 
  select(-model, -color, -text_title, -grid_size) %>% 
  relocate(category, .before = "trees") %>% 
  openxlsx::write.xlsx(., "figs/06_additional/03_model-evaluation/hyperparameters.xlsx")

## 4.2 Performance (RMSE and R²) ----

data_rmse_rsq <- model_results$model_pred_obs %>% 
  drop_na(area) %>% 
  group_by(category) %>% 
  mutate(residual = yhat - y,
         res = sum((y - yhat)^2),
         tot = sum((y - mean(y))^2)) %>% 
  ungroup() %>% 
  group_by(category, area) %>% 
  summarise(res = sum((y - yhat)^2),
            tot = sum((y - mean(y))^2),
            rsq = 1 - (res/tot),
            rmse = sqrt(sum(residual^2/n()))) %>% 
  ungroup() %>% 
  select(-res, -tot)

data_rmse_rsq %>% 
  select(-rsq) %>% 
  pivot_wider(names_from = category, values_from = rmse) %>% 
  arrange(area) %>% 
  select(area, `Hard coral`, `Algae`, `Other fauna`, `Macroalgae`,
         `Coralline algae`, `Turf algae`, `Acropora`, `Orbicella`, `Porites`) %>% 
  # Add entire region values
  bind_rows(., model_results$model_pred_obs %>% 
              group_by(category) %>% 
              mutate(residual = yhat - y,
                     res = sum((y - yhat)^2),
                     tot = sum((y - mean(y))^2)) %>% 
              summarise(res = sum((y - yhat)^2),
                        tot = sum((y - mean(y))^2),
                        rsq = 1 - (res/tot),
                        rmse = sqrt(sum(residual^2/n()))) %>% 
              ungroup() %>% 
              select(category, rmse) %>% 
              pivot_wider(names_from = category, values_from = rmse) %>% 
              mutate(area = "Entire region", .before = 1)) %>% 
  mutate(across(c(`Hard coral`:`Porites`), ~round(.x, 2))) %>% 
  mutate(across(c(`Hard coral`:`Porites`), ~ifelse(.x == -Inf, NA, .x))) %>% 
  openxlsx::write.xlsx(., "figs/06_additional/03_model-evaluation/performance_rmse.xlsx")

data_rmse_rsq %>% 
  select(-rmse) %>% 
  pivot_wider(names_from = category, values_from = rsq) %>% 
  arrange(area) %>% 
  select(area, `Hard coral`, `Algae`, `Other fauna`, `Macroalgae`,
         `Coralline algae`, `Turf algae`, `Acropora`, `Orbicella`, `Porites`) %>% 
  # Add entire region values
  bind_rows(., model_results$model_pred_obs %>% 
              group_by(category) %>% 
              mutate(residual = yhat - y,
                     res = sum((y - yhat)^2),
                     tot = sum((y - mean(y))^2)) %>% 
              summarise(res = sum((y - yhat)^2),
                        tot = sum((y - mean(y))^2),
                        rsq = 1 - (res/tot),
                        rmse = sqrt(sum(residual^2/n()))) %>% 
              ungroup() %>% 
              select(category, rsq) %>% 
              pivot_wider(names_from = category, values_from = rsq) %>% 
              mutate(area = "Entire region", .before = 1)) %>% 
  mutate(across(c(`Hard coral`:`Porites`), ~round(.x, 2))) %>% 
  mutate(across(c(`Hard coral`:`Porites`), ~ifelse(.x == -Inf, NA, .x))) %>% 
  openxlsx::write.xlsx(., "figs/06_additional/03_model-evaluation/performance_rsq.xlsx")

if(FALSE){
  
  # At the regional scale, the RMSE and RSQ don't need to be calculated
  # since they were exported by the "c01_model_bootstrap_xgb.R" script.
  # However, the values cannot be exported per area, so it's necessary to calculate it.
  # Note that the differences in exported and calculated RMSE and RSQ can be due to the
  # filtering of the years at line 36 of this script
  
  model_results$model_performance %>% 
    group_by(category) %>% 
    summarise(across(c(rmse, rsq), ~mean(.x)))
  
}

rm(data_rmse_rsq)

## 4.3 Predicted vs observed ----

plot_pred_obs(all = TRUE)

map(unique(model_results$model_pred_obs$category),
    ~plot_pred_obs(category_i = .))

## 4.4 Residuals ----

plot_residuals(all = TRUE)

map(unique(model_results$model_pred_obs$category),
    ~plot_residuals(category_i = .))

## 4.5 VIMP ----

### 4.5.1 Transform data ----

data_imp_summary <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  group_by(predictor, category, color) %>% 
  summarise(mean = mean(importance),
            lower_ci_95 = quantile(importance, 0.05),
            lower_ci_80 = quantile(importance, 0.10),
            upper_ci_95 = quantile(importance, 0.95),
            upper_ci_80 = quantile(importance, 0.80)) %>% 
  ungroup() %>% 
  group_by(category, color) %>% 
  arrange(desc(mean)) %>% 
  slice_head(n = 25) %>% 
  ungroup()

data_imp_raw <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  left_join(data_imp_summary, .) %>% 
  group_by(category)

### 4.5.2 Map over the function ----

map(unique(data_imp_summary$category), ~plot_vimp(category_i = .))

rm(data_imp_raw)

## 4.6 PDP ----

### 4.6.1 Transform data ----

data_pdp <- model_results$result_pdp

### 4.6.2 Map over the function ----

map(unique(data_pdp$category), ~plot_pdp(category_i = .x))

## 4.7 Maps of predicted values ----

### 4.7.1 Predictions per site and year ----

data_predicted <- model_results$results_predicted %>% 
  # Remove NA (due to not exported results, to save memory)
  drop_na(year) %>% 
  # Create time period
  mutate(time_period = case_when(year %in% seq(1985, 1999) ~ "1985-1999",
                                 year %in% seq(2000, 2014) ~ "2000-2014",
                                 year %in% seq(2015, 2023) ~ "2015-2023")) %>% 
  drop_na(time_period) %>% 
  # Average per time period and category
  group_by(time_period, decimalLatitude, decimalLongitude, category) %>% 
  summarise(measurementValuepred = mean(measurementValuepred)) %>% 
  ungroup() %>% 
  # Convert to sf
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

### 4.7.2 Crop of the region ----

data_crop <- tibble(lon = c(-105, -50), lat = c(6, 38)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

### 4.7.3 Background map ----

data_land_ne <- read_sf("data/01_maps/01_raw/04_natural-earth/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

data_land_ne <- st_intersection(data_land_ne, data_crop)

### 4.7.4 Create the grid ----

data_grid <- st_make_grid(data_crop, n = 150, crs = 4326) %>% 
  st_as_sf() %>% 
  mutate(cell_id = 1:nrow(.))

### 4.7.5 Summarize sites values per grid cell ----

data_predicted <- st_join(data_predicted, data_grid) %>% 
  group_by(time_period, category, cell_id) %>% 
  summarise(cover_pred = mean(measurementValuepred)) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  left_join(., data_grid) %>% 
  st_as_sf()

### 4.7.6 Map over the function ----

map(unique(data_predicted$category), ~plot_prediction_map(category_i = .x))

# 5. Temporal trends ----

## 5.1 Load and transform obs data ----

load("data/09_model-data/data_benthic_prepared.RData")

data_benthic <- data_benthic %>% 
  add_colors()

data_benthic <- data_benthic %>% 
  bind_rows(., data_benthic %>% 
              mutate(area = "All")) %>% 
  group_by(year, area, category, color, text_title) %>% 
  summarise(mean = mean(measurementValue),
            sd = sd(measurementValue)) %>% 
  ungroup() %>% 
  mutate(ymin = mean - sd,
         ymin = ifelse(ymin < 0, 0, ymin),
         ymax = mean + sd) %>% 
  bind_rows(., data_benthic %>% 
              group_by(year, category, color, text_title) %>% 
              summarise(mean = mean(measurementValue),
                        sd = sd(measurementValue)) %>% 
              ungroup() %>% 
              mutate(ymin = mean - sd,
                     ymin = ifelse(ymin < 0, 0, ymin),
                     ymax = mean + sd,
                     area = "Caribbean")) %>% 
  complete(year, category, nesting(area), fill = list(mean = NA, sd = NA)) %>% 
  select(-color, -text_title) %>% 
  left_join(., data_benthic %>% 
              select(category, text_title, color) %>% 
              distinct()) %>% 
  drop_na(area)

## 5.2 Map over the function -----

map(unique(data_trends$smoothed_trends$area),
    ~plot_trends(area_i = .x,
                 icons = TRUE,
                 scales = "fixed",
                 raw_data = TRUE,
                 modelled_data = TRUE))

# Raw data (for writing)

if(FALSE){
  
  A <- data_trends$smoothed_trends %>%
    filter(area == "All" & category == "Acropora") %>%
    select(-upper_ci_95, -lower_ci_95, -text_title, -color)

}

# 8. Hard coral vs algae cover ----

## 8.1 Transform data ----

data_ex_summ <- data_trends$raw_trends %>% 
  filter(area == "All" & category %in% c("Hard coral", "Algae")) %>% 
  select(year, category, mean) %>% 
  pivot_wider(names_from = "category", values_from = "mean") %>% 
  mutate(ratio = `Hard coral`/`Algae`)

## 8.2 Create labels ----

data_labels <- tibble(type = c(1, 1, 2),
                      x = c(30, 10, 50),
                      y = c(10, 30, 50),
                      text = c("**More <span style='color:#16a085'>algae</span>**<br>than <span style='color:#c44d56'>hard corals</span>",
                               "**More <span style='color:#c44d56'>hard corals</span>**<br>than <span style='color:#16a085'>algae</span>",
                               "As much <span style='color:#c44d56'>hard corals</span> than <span style='color:#16a085'>algae</span>"))

## 8.3 Make the plot ----

ggplot(data = data_ex_summ, aes(x = Algae, y = `Hard coral`, label = year)) +
  geom_line() +
  geom_text_repel(data = data_ex_summ %>% filter(year %in% c(1985, 2000, 2010, 2023)),
                  aes(x = Algae, y = `Hard coral`, label = year), force = 40,
                  family = font_choose_graph, seed = 27, min.segment.length = unit(10, "cm")) + 
  geom_point(data = data_ex_summ, aes(x = Algae, y = `Hard coral`, fill = ratio),
             size = 2, shape = 21, show.legend = FALSE, color = "black") +
  geom_point(data = data_ex_summ %>% filter(year %in% c(1985, 2000, 2010, 2023)),
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

ggsave("figs/01_part-1/fig-16.png", width = 6, height = 6, dpi = fig_resolution)

# 9. Figures for Executive Summary ----

## 9.1 Hard coral cover ----

data_trends$smoothed_trends %>% 
  filter(category == "Hard coral" & area == "All") %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = palette_first[2]), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, color = palette_first[2]), linewidth = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1985, NA)) +
  scale_y_continuous(limits = c(0, 40)) +
  # 1980's white band disease
  annotate("rect", xmin = 1985, xmax = 1990, ymin = 10, ymax = 10.75, fill = 'gray') +
  annotate("text", x = 1985.25, y = 12.25, fill = 'gray', label = "1980's", family = "Open Sans Semibold",
           family = font_choose_graph, color = palette_first[2], face = "bold", hjust = 0, size = 3) +
  annotate("text", x = 1985.25, y = 8.5, fill = 'gray', label = "WBD",
           family = font_choose_graph, color = "black", face = "bold", hjust = 0, size = 3) +
  # 1990's white plague
  annotate("rect", xmin = 1990, xmax = 2000, ymin = 9.25, ymax = 10, fill = 'gray') +
  annotate("text", x = 1990.25, y = 11.5, fill = 'gray', label = "1990's", family = "Open Sans Semibold",
           family = font_choose_graph, color = palette_first[2], face = "bold", hjust = 0, size = 3) +
  annotate("text", x = 1990.25, y = 7.5, fill = 'gray', label = "WP",
           family = font_choose_graph, color = "black", face = "bold", hjust = 0, size = 3) +
  # 1998 bleaching event
  annotate("rect", xmin = 1998, xmax = 1998.35, ymin = 30, ymax = 35, fill = 'gray') +
  annotate("text", x = 1998.75, y = 34, fill = 'gray', label = "1998", family = "Open Sans Semibold",
           family = font_choose_graph, color = palette_first[2], face = "bold", hjust = 0, size = 3) +
  annotate("text", x = 1998.75, y = 31.5, fill = 'gray', label = "Bleaching event",
           family = font_choose_graph, color = "black", face = "bold", hjust = 0, size = 3) +
  # 2005 bleaching event
  annotate("rect", xmin = 2005, xmax = 2005.35, ymin = 25, ymax = 30, fill = 'gray') +
  annotate("text", x = 2005.75, y = 29, fill = 'gray', label = "2005", family = "Open Sans Semibold",
           family = font_choose_graph, color = palette_first[2], face = "bold", hjust = 0, size = 3) +
  annotate("text", x = 2005.75, y = 26.5, fill = 'gray', label = "Bleaching event",
           family = font_choose_graph, color = "black", face = "bold", hjust = 0, size = 3) +
  # 2014 - 2022 SCTLD
  annotate("rect", xmin = 2014, xmax = 2022, ymin = 9.25, ymax = 10, fill = 'gray') +
  annotate("text", x = 2014, y = 11.5, fill = 'gray', label = "2014-2022", family = "Open Sans Semibold",
           family = font_choose_graph, color = palette_first[2], face = "bold", hjust = 0, size = 3) +
  annotate("text", x = 2014, y = 7.5, fill = 'gray', label = "SCTLD",
           family = font_choose_graph, color = "black", face = "bold", hjust = 0, size = 3) +
  labs(title = paste0("Changes in <span style = 'color: ",
                      palette_first[2],
                      "'>hard coral cover</span> in the Caribbean<br>between 1985 and 2022"),
       x = "Year", y = "Hard coral cover (%)",
       subtitle = "<br><span style = 'color: #24252a'>The bold line represent the average,
       the ribbon<br>represent the confidence interval of 95%</span>") + 
  theme(plot.title = element_markdown(size = 17, face = "bold", family = "Open Sans Semibold"),
        plot.subtitle = element_markdown(size = 12))

ggsave("figs/00_misc/exe-summ_1.png", height = 5.3, width = 7.2, dpi = fig_resolution)

## 9.2 Overall benthic cover ----

### 9.2.1 Transform data ----

data_ex_summ <- data_trends$smoothed_trends %>% 
  filter(area == "All" & category %in% c("Hard coral", "Algae", "Other fauna"))

data_ex_summ <- data_ex_summ %>% 
  group_by(year) %>% 
  summarise(mean = sum(mean)) %>% 
  ungroup() %>% 
  # Generate the others category (difference between 100 and sum of all categories)
  mutate(mean = 100 - mean, 
         category = "Others",
         color = "lightgrey") %>% 
  bind_rows(data_ex_summ, .)

data_labels <- tibble(x = c(1995, 1995, 1995, 1995),
                      y = c(82, 52.5, 36, 14),
                      category = c("Algae", "Hard coral", "Other fauna", "Others"),
                      color = c("white", "white", "white", "black"))

### 9.2.2 Make the plot ----

ggplot(data = data_ex_summ, aes(x = year, y = mean, fill = category)) +
  geom_area(show.legend = FALSE, color = "white", outline.type = "full", linewidth = 0.25) +
  scale_fill_manual(values = unique(data_ex_summ$color)) +
  theme_graph() +
  geom_text(data = data_labels, aes(x = x, y = y, label = category, color = color),
            size = 4.5, family = font_choose_graph) +
  scale_color_identity() +
  labs(x = "Year", y = "Benthic cover (%)")

ggsave("figs/00_misc/exe-summ_2.png", width = 7, height = 5, dpi = fig_resolution)
