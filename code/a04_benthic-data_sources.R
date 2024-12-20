# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)

# 2. Load data ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

load("data/02_misc/data-benthic.RData")

# 3. DatasetID per territory ----

data_benthic %>% 
  select(area, datasetID) %>% 
  distinct() %>% 
  arrange(area, datasetID) %>% 
  group_by(area) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  left_join(data_area %>% st_drop_geometry(), .) %>% 
  arrange(area) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-1.xlsx")

# 4. List of contributors per datasetID ----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name, email) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-2.xlsx")

# 5. List of contributors emails ----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(last_name, first_name, email) %>% 
  distinct() %>% 
  arrange(last_name) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-3.xlsx")

# 6. List of contributors per area ----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, last_name, first_name) %>% 
  full_join(data_benthic %>% 
              select(datasetID, area) %>% 
              distinct(),
            .) %>% 
  drop_na(last_name) %>% 
  arrange(area, last_name) %>% 
  mutate(last_name = str_to_title(last_name),
         name = paste0(first_name, " ", last_name)) %>% 
  select(-datasetID, -last_name, -first_name) %>% 
  distinct() %>% 
  group_by(area) %>% 
  mutate(name = paste0(name, collapse = ", ")) %>% 
  distinct() %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-4.xlsx")
