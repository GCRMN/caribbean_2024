# 1. Load packages ----

library(tidyverse)
library(readxl)

# 2. Load data ----

load("data/02_misc/data-benthic.RData")

# 3. DatasetID per territory ----

data_benthic %>% 
  select(area, datasetID) %>% 
  distinct() %>% 
  arrange(area, datasetID) %>% 
  group_by(area) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-1.xlsx")

# 4. List of contributors ----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-2.xlsx")
