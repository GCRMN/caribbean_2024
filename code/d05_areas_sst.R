# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(RcppRoll)
library(sf)
library(scales)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/extract_coeff.R")

theme_set(theme_graph())

# 3. Load data ----

data_warming <- read.csv("data/02_misc/data-warming.csv") 

# 4. Export tables ----

## 4.1 Transform the table ----

data_table_3 <- data_warming %>% 
  filter(!(area %in% c("Entire Caribbean region", "Navassa Island"))) %>% 
  bind_rows(., data_warming %>% 
              filter(area == "Entire Caribbean region")) %>% 
  select(area, mean_sst, sst_increase, warming_rate) %>% 
  mutate(mean_sst = format(round(mean_sst, 2), nsmall = 2),
         sst_increase = format(round(sst_increase, 2), nsmall = 2),
         warming_rate = format(round(warming_rate, 3), nsmall = 2))

## 4.2 Export as .xlsx ----

openxlsx::write.xlsx(data_table_3, file = "figs/01_part-1/tbl-3.xlsx")

# 5. SST (year) for each area ----

## 5.1 Transform the data ----

load("data/02_misc/data-sst_processed.RData")

data_sst <- data_sst %>% 
  filter(!(area %in% c("Entire Caribbean region", "Navassa Island")))
  
data_sst <- data_sst %>% 
  left_join(., data_warming %>% select(-mean_sst), by = "area") %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_linear = slope*date_num+intercept) %>% 
  mutate(daymonth = str_sub(date, 6, 10),
         year = year(date),
         area = str_replace_all(area, "Saint Vincent and the Grenadines", "St. Vincent & the Grenadines"))

## 5.2 Make the plots ----

ggplot(data = data_sst %>% 
         filter(area %in% sort(unique(data_sst$area))[1:15]),
       aes(x = date, y = sst)) +
  geom_line(color = "#2c3e50", linewidth = 0.25) +
  geom_line(aes(x = date, y = sst_linear), color = palette_second[2], linewidth = 0.8) +
  geom_hline(aes(yintercept = mean_sst), color = palette_second[4], linewidth = 0.8) +
  labs(x = "Year", y = "Sea Surface Temperature (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) + 
  facet_wrap(~area, ncol = 3, scales = "free") +
  theme_graph() +
  theme(strip.text = element_text(hjust = 0.5),
        strip.background = element_blank())

ggsave(filename = "figs/05_supp-mat/supp_fig_1_a.png", width = 10, height = 12, dpi = fig_resolution)

ggplot(data = data_sst %>% 
         filter(area %in% sort(unique(data_sst$area))[16:30]),
       aes(x = date, y = sst)) +
  geom_line(color = "#2c3e50", linewidth = 0.25) +
  geom_line(aes(x = date, y = sst_linear), color = palette_second[2], linewidth = 0.8) +
  geom_hline(aes(yintercept = mean_sst), color = palette_second[4], linewidth = 0.8) +
  labs(x = "Year", y = "Sea Surface Temperature (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) + 
  facet_wrap(~area, ncol = 3, scales = "free") +
  theme_graph() +
  theme(strip.text = element_text(hjust = 0.5),
        strip.background = element_blank())

ggsave(filename = "figs/05_supp-mat/supp_fig_1_b.png", width = 10, height = 12, dpi = fig_resolution)

ggplot(data = data_sst %>% 
         filter(area %in% sort(unique(data_sst$area))[31:43]),
       aes(x = date, y = sst)) +
  geom_line(color = "#2c3e50", linewidth = 0.25) +
  geom_line(aes(x = date, y = sst_linear), color = palette_second[2], linewidth = 0.8) +
  geom_hline(aes(yintercept = mean_sst), color = palette_second[4], linewidth = 0.8) +
  labs(x = "Year", y = "Sea Surface Temperature (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) + 
  facet_wrap(~area, ncol = 3, scales = "free") +
  theme_graph() +
  theme(strip.text = element_text(hjust = 0.5),
        strip.background = element_blank())

ggsave(filename = "figs/05_supp-mat/supp_fig_1_c.png", width = 10, height = 9, dpi = fig_resolution)

# 6. SST (month) for each area ----

## 6.1 Transform the data ----

data_sst_month <- data_sst %>% 
  mutate(daymonth = str_sub(date, 6, 10),
         year = year(date),
         decade = case_when(year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 & year < 2020 ~ "2010s",
                            year >= 2020 & year < 2030 ~ "2020s"),
         area = str_replace_all(area, "Saint Vincent and the Grenadines", "St. Vincent & the Grenadines")) %>% 
  arrange(decade)

data_sst_month_mean <- data_sst_month %>% 
  group_by(daymonth, area) %>% 
  summarise(sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = "all")

## 6.2 Make the plots ----

ggplot() +
  geom_line(data = data_sst_month %>% filter(area %in% sort(unique(data_sst_month$area))[1:15]),
            aes(x = daymonth, y = sst, group = year, color = decade),
            alpha = 0.75, linewidth = 0.5) +
  geom_line(data = data_sst_month_mean %>% filter(area %in% sort(unique(data_sst_month$area))[1:15]),
            aes(x = daymonth, y = sst, group = year),
            color = "black", linewidth = 1) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                              "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"), 
                   labels = c("", "Feb.", "", "Apr.", "", "Jun.", "", "Aug.", 
                              "", "Oct.", "", "Dec.")) +
  labs(x = "Month", y = "SST (°C)") + 
  theme(legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  scale_color_manual(name = "Decade", values = palette_second) +
  guides(color = guide_legend(override.aes = list(linewidth = 1))) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
  facet_wrap(~area, ncol = 3, scales = "free_y")

ggsave(filename = "figs/05_supp-mat/supp_fig_2_a.png", width = 10, height = 13, dpi = fig_resolution)

ggplot() +
  geom_line(data = data_sst_month %>% filter(area %in% sort(unique(data_sst_month$area))[16:30]),
            aes(x = daymonth, y = sst, group = year, color = decade),
            alpha = 0.75, linewidth = 0.5) +
  geom_line(data = data_sst_month_mean %>% filter(area %in% sort(unique(data_sst_month$area))[16:30]),
            aes(x = daymonth, y = sst, group = year),
            color = "black", linewidth = 1) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                              "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"), 
                   labels = c("", "Feb.", "", "Apr.", "", "Jun.", "", "Aug.", 
                              "", "Oct.", "", "Dec.")) +
  labs(x = "Month", y = "SST (°C)") + 
  theme(legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  scale_color_manual(name = "Decade", values = palette_second) +
  guides(color = guide_legend(override.aes = list(linewidth = 1))) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
  facet_wrap(~area, ncol = 3, scales = "free_y")

ggsave(filename = "figs/05_supp-mat/supp_fig_2_b.png", width = 10, height = 13, dpi = fig_resolution)

ggplot() +
  geom_line(data = data_sst_month %>% filter(area %in% sort(unique(data_sst_month$area))[31:43]),
            aes(x = daymonth, y = sst, group = year, color = decade),
            alpha = 0.75, linewidth = 0.5) +
  geom_line(data = data_sst_month_mean %>% filter(area %in% sort(unique(data_sst_month$area))[31:43]),
            aes(x = daymonth, y = sst, group = year),
            color = "black", linewidth = 1) +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                              "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"), 
                   labels = c("", "Feb.", "", "Apr.", "", "Jun.", "", "Aug.", 
                              "", "Oct.", "", "Dec.")) +
  labs(x = "Month", y = "SST (°C)") + 
  theme(legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  scale_color_manual(name = "Decade", values = palette_second) +
  guides(color = guide_legend(override.aes = list(linewidth = 1))) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
  facet_wrap(~area, ncol = 3, scales = "free_y")

ggsave(filename = "figs/05_supp-mat/supp_fig_2_c.png", width = 10.25, height = 9.5, dpi = fig_resolution)

# 7. SST anomaly (trend) for each area ----

load("data/02_misc/data-sst_processed.RData")

data_sst <- data_sst %>% 
  filter(!(area %in% c("Entire Caribbean region", "Navassa Island"))) %>% 
  drop_na(sst_anom_mean)

data_sst <- data_sst %>% 
  mutate(date = as.numeric(as_date(date))) %>% 
  group_by(area) %>% 
  # Extract linear model coefficients
  group_modify(~extract_coeff(data = .x, var_y = "sst_anom_mean", var_x = "date")) %>% 
  ungroup() %>% 
  left_join(data_sst, .) %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_anom_mean_linear = slope*date_num+intercept)

## 7.1 Create the function for the base plot ----

base_plot <- function(area_i){
  
  data_i <- data_sst %>% 
    filter(area %in% area_i)
  
  plot_i <- ggplot(data = data_i) +
    geom_ribbon(data = data_i %>% mutate(sst_anom_mean = if_else(sst_anom_mean < sst_anom_mean_linear,
                                                                 sst_anom_mean_linear,
                                                                 sst_anom_mean)),
                aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill = "#d35f5fff", alpha = 0.9) +
    geom_ribbon(data = data_i %>% mutate(sst_anom_mean = if_else(sst_anom_mean > sst_anom_mean_linear,
                                                                 sst_anom_mean_linear,
                                                                 sst_anom_mean)),
                aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
    geom_line(aes(x = date, y = sst_anom_mean_linear)) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 14)) +
    labs(x = "Year", y = "SST anomaly (°C)") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))
  
  return(plot_i)
  
}

## 7.2 Create the function to produce the plots ----

map_sst_anom <- function(group_area_i){
  
    base_plot(area_i = group_area_i)
    
    ggsave(filename = paste0("figs/02_part-2/fig-2/",
                             str_replace_all(str_replace_all(str_to_lower(group_area_i), " ", "-"), "---", "-"), ".png"),
           width = 6, height = 4, dpi = fig_resolution)
    
}

## 7.3 Map over the function ----

map(data_sst %>% 
      select(area) %>% 
      distinct() %>% 
      pull(),
    ~map_sst_anom(.))

# 8. SST anomaly for each area ----

## 8.1 Transform the data ----

load("data/02_misc/data-sst_processed.RData")

data_sst <- data_sst %>% 
  filter(!(area %in% c("Entire Caribbean region", "Navassa Island"))) %>% 
  mutate(area = str_replace_all(area, "Saint Vincent and the Grenadines", "St. Vincent & the Grenadines"))

## 8.2 Make the plots ----

data_sst_anom <- data_sst %>% 
  drop_na(sst_anom_mean) %>%
  filter(area %in% sort(unique(data_sst$area))[1:15])

ggplot(data = data_sst_anom) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = 0)) +
  facet_wrap(~area, ncol = 3, scales = "free_y") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

ggsave(filename = "figs/05_supp-mat/supp_fig_3_a.png", width = 10, height = 12, dpi = fig_resolution)

data_sst_anom <- data_sst %>% 
  drop_na(sst_anom_mean) %>%
  filter(area %in% sort(unique(data_sst$area))[16:30])

ggplot(data = data_sst_anom) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                                      0,
                                                                      sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0,
                                                                      0,
                                                                      sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = 0)) +
  facet_wrap(~area, ncol = 3, scales = "free_y") +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

ggsave(filename = "figs/05_supp-mat/supp_fig_3_b.png", width = 10, height = 12, dpi = fig_resolution)

data_sst_anom <- data_sst %>% 
  drop_na(sst_anom_mean) %>%
  filter(area %in% sort(unique(data_sst$area))[31:43])

ggplot(data = data_sst_anom) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                                      0,
                                                                      sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0,
                                                                      0,
                                                                      sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = 0)) +
  facet_wrap(~area, ncol = 3, scales = "free_y") +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

ggsave(filename = "figs/05_supp-mat/supp_fig_3_c.png", width = 10, height = 9, dpi = fig_resolution)
