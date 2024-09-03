# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)

# 2. Load data ----

load("data/02_misc/data-benthic.RData")

# 3. DatasetID per territory ----

data_benthic %>% 
  select(territory, datasetID) %>% 
  distinct() %>% 
  arrange(territory, datasetID) %>% 
  group_by(territory) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-1.xlsx")
