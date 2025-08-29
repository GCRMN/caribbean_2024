# 1. Load packages ----

library(tidyverse)
library(rmarkdown)
library(sf)
library(googledrive)

# 2. Load the areas to map over ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp") %>% 
  st_drop_geometry() %>% 
  filter(!(area %in% c("Navassa Island"))) %>% 
  distinct() %>% 
  arrange(area) %>% 
  mutate(nb = row_number())

# 3. Create the folders ----

if(FALSE){
  
  map(str_replace_all(str_replace_all(str_to_lower(data_area$area), " ", "-"), "---", "-"),
      ~drive_mkdir(path = "GCRMN Caribbean report/07_part-2_syntheses-countries-territories", name = .x))
  
}

# 4. Load the figures to the folders ----

## 4.1 Figure 1 ----

map(str_replace_all(str_replace_all(str_to_lower(data_area$area), " ", "-"), "---", "-"),
    ~drive_upload(media = paste0("figs/02_part-2/fig-1/", .x, ".png"),
                  path = paste0("GCRMN Caribbean report/07_part-2_syntheses-countries-territories/", .x),
                  name = "fig-1"))
  
## 4.2 Figure 2 ----

map(str_replace_all(str_replace_all(str_to_lower(data_area$area), " ", "-"), "---", "-"),
    ~drive_upload(media = paste0("figs/02_part-2/fig-2/", .x, ".png"),
                  path = paste0("GCRMN Caribbean report/07_part-2_syntheses-countries-territories/", .x),
                  name = "fig-2"))

## 4.3 Figure 3 ----

map(str_replace_all(str_replace_all(str_to_lower(data_area$area), " ", "-"), "---", "-"),
    ~drive_upload(media = paste0("figs/02_part-2/fig-3/", .x, ".png"),
                  path = paste0("GCRMN Caribbean report/07_part-2_syntheses-countries-territories/", .x),
                  name = "fig-3"))

## 4.4 Figure 4 ----

map(str_replace_all(str_replace_all(str_to_lower(data_area$area), " ", "-"), "---", "-"),
    ~drive_upload(media = paste0("figs/02_part-2/fig-4/", .x, ".png"),
                  path = paste0("GCRMN Caribbean report/07_part-2_syntheses-countries-territories/", .x),
                  name = "fig-4"))

## 4.5 Figure 5 ----

map(str_replace_all(str_replace_all(str_to_lower(data_area$area), " ", "-"), "---", "-"),
    ~drive_upload(media = paste0("figs/02_part-2/fig-5/", .x, ".png"),
                  path = paste0("GCRMN Caribbean report/07_part-2_syntheses-countries-territories/", .x),
                  name = "fig-5"))

## 4.6 Figure 5b ----

map(list.files("figs/02_part-2/fig-5b/", full.names = TRUE),
    ~drive_upload(media = .x,
                  path = paste0("GCRMN Caribbean report/07_part-2_syntheses-countries-territories/",
                                str_split_fixed(.x, "/|\\.", 5)[,4]),
                  name = "fig-5b"))

## 4.7 Docx chapter ----

map(list.files("doc/", full.names = TRUE, pattern = ".docx$"),
    ~drive_upload(media = .x,
                  path = paste0("GCRMN Caribbean report/07_part-2_syntheses-countries-territories/",
                                str_split_fixed(.x, "_|\\.", 3)[,2]),
                  name = paste0("chapter_", str_split_fixed(.x, "_|\\.", 3)[,2])))
