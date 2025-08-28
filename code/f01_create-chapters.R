# 1. Load packages ----

library(tidyverse)
library(rmarkdown)
library(sf)

# 2. Load the areas to map over ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp") %>% 
  st_drop_geometry() %>% 
  filter(!(area %in% c("Navassa Island"))) %>% 
  distinct() %>% 
  arrange(area) %>% 
  mutate(nb = row_number())

# 3. Load cyclones data ----

load("data/07_cyclones/02_cyclones_extracted.RData")

# 4. Create the function to render the docx documents ----

render_rmd <- function(area_i){
  
  require(rmarkdown)

  territory_name <- str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),  "---", "-")
  
  nb_chapter <- data_area %>% filter(area == area_i) %>% select(nb) %>% pull()
  
  file_name <- paste0(str_pad(nb_chapter, width = 2, pad = "0"), "_", territory_name, ".docx")
  
  if(file.exists(paste0("doc/", file_name)) == FALSE){
    
    render("code/function/chapter_docx.Rmd", 
           output_file = file_name,
           output_dir = "doc/",
           quiet = TRUE)
    
  }

}

# 5. Map over the function ----

map(data_area$area, ~render_rmd(area_i = .))
