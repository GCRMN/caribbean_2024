# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(treemapify)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Table 1 - Geographic information ----

## 3.1 Maritime area ----

data_eez <- st_read("data/01_shp/02_clean/03_eez/caribbean_eez.shp")

data_maritime_area <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, AREA_KM2) %>% 
  rename(territory = TERRITORY1, maritime_area = AREA_KM2) %>% 
  arrange(territory)
