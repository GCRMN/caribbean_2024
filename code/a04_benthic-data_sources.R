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

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name, email) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-2.xlsx")

# 5. List of contributors emails ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(last_name, first_name, email) %>% 
  distinct() %>% 
  arrange(last_name) %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-3.xlsx")

# 6. List of contributors per area ----

## 6.1 Data contributors for AGRRA (datasetID 0091) ----

### 6.1.1 Load AGRRA raw data (to get batch and lat/long) ----

data_agrra_raw <- read_xlsx("../../2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/01_raw-data/0091/BenthicPointCoverByTransect.xlsx",
                            sheet = 2) %>% 
  select(Batch, Latitude, Longitude) %>% 
  distinct()

data_agrra_raw <- read_xlsx("../../2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/01_raw-data/0091/BenthicCoralCoverSpeciesByTransect.xlsx",
                            sheet = 2) %>% 
  select(Batch, Latitude, Longitude) %>% 
  bind_rows(data_agrra_raw, .) %>% 
  distinct()

### 6.1.2 Load AGRRA data contributors ----

data_agrra <- read_xlsx("../../2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/01_raw-data/0091/AGRRA_Surveyors_Feb2025 updated for GCRMN.xlsx",
                        sheet = 2) %>% 
  left_join(data_agrra_raw, .)

### 6.1.3 Spatial join with area ----

data_agrra <- data_agrra %>% 
  select(Latitude, Longitude, Name, `Org Name`) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_intersection(., data_area) %>% 
  st_drop_geometry() %>% 
  select(area, Name, Org.Name) %>% 
  distinct() %>% 
  drop_na(Name) %>% 
  mutate(Name = str_remove_all(Name, '"Max"'),
         Name = str_replace_all(Name, c("\\(Richards\\)" = "R.",
                                        "\\(Fisher\\)" = "F."))) %>% 
  separate_longer_delim(Name, "/") %>% 
  mutate(agrra = TRUE,
         Org.Name = case_when(Org.Name %in% c("[Not Applicable]", "Independent",
                                              "INDEPENDENT", "AGRRA volunteer", "[INDEPENDENT") ~ NA_character_,
                              TRUE ~ Org.Name),
         first_name = str_split_fixed(Name, " ", 2)[,1],
         last_name = str_split_fixed(Name, " ", 2)[,2],
         datasetID = "0091") %>% 
  select(datasetID, area, agrra, last_name, first_name) %>% 
  distinct()

## 6.2 Non AGRRA data contributors ----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  filter(!(datasetID %in% c("0015", "0091"))) %>% # Remove aggregated datasets
  select(datasetID, last_name, first_name) %>% 
  full_join(data_benthic %>% 
              select(datasetID, area) %>% 
              distinct(),
            .) %>% 
  drop_na(last_name) %>% 
  bind_rows(., data_agrra) %>% 
  arrange(area, last_name) %>% 
  mutate(last_name = str_to_title(last_name),
         name = case_when(agrra == TRUE ~ paste0(first_name, " ", last_name, "*"),
                          TRUE ~ paste0(first_name, " ", last_name))) %>% 
  select(-datasetID, -last_name, -first_name, -agrra) %>% 
  distinct() %>% 
  group_by(area) %>% 
  mutate(name = paste0(name, collapse = ", ")) %>% 
  distinct() %>% 
  openxlsx::write.xlsx(., file = "figs/05_supp-mat/tbl-4.xlsx")

# 7. Acknowledgments and citations to include -----

read_xlsx("C:/Users/jerem/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, citation, acknowledgments) %>% 
  distinct() %>% 
  filter(!(is.na(citation) & is.na(acknowledgments)))
