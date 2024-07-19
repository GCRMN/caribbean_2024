# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2. Source functions ----

source("code/function/plot_region.R")

# 3. Create annotations ----

data_annotations <- tibble(long = c(-59, -95),
                            lat = c(26, 10),
                            text = c("Atlantic Ocean", "Pacific Ocean")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# 4. Make the plot ----

plot <- plot_region(scale = TRUE) +
  geom_sf_text(data = data_annotations, aes(label = text), fontface = "italic",
               color = "white", size = 3, family = font_choose_map)

# 5. Export the map ----

ggsave(filename = "figs/01_part-1/fig-1.png", plot = plot,
       width = 7.25, height = 4.75, dpi = fig_resolution)
