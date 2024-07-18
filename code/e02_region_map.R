# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2. Source functions ----

source("code/function/plot_region.R")

# 3. Make the plot ----

plot <- plot_region() +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = font_choose_map, text_col = "black",
                   text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"), line_col = "black",
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))

# 4. Export the map ----

ggsave(filename = "figs/01_part-1/fig-1.png", plot = plot,
       width = 7.25, height = 4.75, dpi = fig_resolution)
