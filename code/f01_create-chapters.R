# 1. Load packages ----

library(tidyverse)
library(rmarkdown)
library(sf)

# 2. Load functions ----

source("code/function/render_qmd.R")

# 3. Get list of areas ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp") %>% 
  st_drop_geometry() %>% 
  arrange(area) %>% 
  mutate(nb = row_number())

# 4. Create docx files for each area ----

map(unique(data_area$area), ~render_qmd(area_i = .x))

render_qmd(area_i = "Grenada")
