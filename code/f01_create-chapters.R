# 1. Load packages ----

library(tidyverse)
library(rmarkdown)
library(sf)
library(googledrive)

# 2. Load functions ----

source("code/function/render_qmd.R")

# 3. Load data ----

load("data/07_cyclones/02_cyclones_extracted.RData")

# 4. Create docx files for each area ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp") %>% 
  st_drop_geometry() %>% 
  filter(!(area %in% c("Navassa Island", "Guatemala"))) %>% 
  distinct() %>% 
  arrange(area) %>% 
  mutate(nb = row_number())

map(data_area$area, ~render_qmd(area_i = ., upload_drive = TRUE))

#render_qmd(area_i = "Guadeloupe", upload_drive = FALSE)
