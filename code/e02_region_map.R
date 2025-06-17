# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2. Source functions ----

source("code/function/plot_region.R")

# 3. Make the plot ----

plot <- plot_region(scale = TRUE)

# 4. Export the map ----

ggsave(filename = "figs/01_part-1/fig-1_raw.png", plot = plot,
       width = 7.25, height = 4.75, dpi = fig_resolution)
