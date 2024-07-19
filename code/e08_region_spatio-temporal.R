# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2. Source functions ----

source("code/function/plot_region.R")
source("code/function/data_descriptors.R")
source("code/function/theme_graph.R")

# 3. Select benthic data ----

load("data/02_misc/data-benthic.RData")

data_benthic <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 4. Make the plot ----

plot <- plot_region(scale = TRUE) +
  geom_sf(data = data_benthic %>% arrange(interval_class), aes(color = interval_class)) +
    scale_color_manual(values = palette_second,
                       labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                       drop = FALSE, name = "Number of years with data") +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 4))) +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

ggsave(filename = "figs/01_part-1/fig-2.png", plot = plot,
       width = 7.5, height = 5.75, dpi = fig_resolution)

# 5. Plot of number of surveys per year ----

## 5.1 Make the plot ----

load("data/02_misc/data-benthic.RData")

data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  complete(year, fill = list(n = 0)) %>% 
  mutate(percent = n*100/sum(n)) %>% 
  ggplot(data = ., aes(x = year, y = percent)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 1,
             color = palette_first[4], fill = palette_first[3]) +
    labs(x = "Year", y = "Surveys (%)") +
    coord_cartesian(clip = "off") +
    theme_graph() +
    scale_x_continuous(expand = c(0, 0), limits = c(1980, NA))

## 5.2 Save the plot ----

ggsave(filename = "figs/01_part-1/fig-10.png", width = 5, height = 4, dpi = fig_resolution)

# 6. Plot of number of surveys per depth ----

## 6.1 Make the plot ----

data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year, verbatimDepth) %>% 
  st_drop_geometry() %>% 
  drop_na(verbatimDepth) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = verbatimDepth)) +
  geom_histogram(binwidth = 1, aes(y = after_stat(width * density * 100)),
                 color = palette_first[4], fill = palette_first[3]) +
  labs(x = "Depth (m)", y = "Surveys (%)") +
  coord_cartesian(clip = "off") +
  theme_graph()

## 6.2 Save the plot ----

ggsave(filename = "figs/01_part-1/fig-11.png", width = 5, height = 4, dpi = fig_resolution)

# 7. Extract monitoring descriptors ----

load("data/02_misc/data-benthic.RData")

## 7.1 Add subterritories ----

monitoring_descriptors <- data_benthic %>% 
  group_by(territory) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  # Add missing territories (those with no data)
  full_join(., st_read("data/01_maps/02_clean/03_eez/caribbean_eez.shp") %>%
              select(territory) %>% 
              distinct() %>% 
              st_drop_geometry()) %>% 
  mutate(across(c("nb_sites", "nb_surveys", "nb_datasets"), .fns = ~replace_na(.,0))) %>% 
  arrange(territory) %>% 
  filter(!(territory %in% c("Quitasueño Bank", "Serrana Bank", "Navassa Island"))) %>% 
  distinct()

## 7.2 Add total ----

monitoring_descriptors <- data_benthic %>% 
  filter(!(territory %in% c("Quitasueño Bank", "Serrana Bank", "Navassa Island"))) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(territory = "Entire Caribbean region") %>% 
  bind_rows(monitoring_descriptors, .)

## 7.3 Export the table ----

openxlsx::write.xlsx(monitoring_descriptors, file = "figs/01_part-1/table-4.xlsx")
