# 1. Required packages ----

library(extrafont)
library(scico)

# 2. Set the default font family ----

windowsFonts("Open Sans" = windowsFont("Open Sans"))
windowsFonts("Open Sans Semibold" = windowsFont("Open Sans Semibold"))

font_choose_graph <- "Open Sans"
font_choose_map <- "Open Sans"

fig_resolution <- 300

# 3. Set the colors ----

palette_first <- scico(5, palette = "oslo", begin = 0.8, end = 0)
palette_second <- c("#fac484", "#f8a07e", "#ce6693", "#a059a0", "#5c53a5")

# palette_second taken from https://carto.com/carto-colors/ (SunsetDark)
