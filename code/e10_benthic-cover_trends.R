# 1. Load packages ----

library(tidyverse)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/data_descriptors.R")

# 3. Select benthic data ----

load("data/02_misc/data-benthic.RData")

# 4. Transform data ----

data_year_dataset <- data_benthic %>% 
  group_by(datasetID, territory, year) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  select(datasetID, territory, year, nb_sites) %>% 
  complete(nesting(datasetID, territory),
           year = 1980:2024,
           fill = list(nb_sites = 0))

# 5. Create a function to produce the plot ----

plot_year_dataset <- function(territory_i){
  
  data_year_dataset_i <- data_year_dataset %>% 
    filter(territory == territory_i)
  
  plot_i <- ggplot(data = data_year_dataset_i,
                   aes(x = year, y = datasetID, fill = nb_sites)) +
    geom_tile() +
    theme_graph() +
    labs(y = NULL, x = "Year") +
    scale_fill_gradientn(colours = c("lightgrey", palette_second),
                         values = c(0, .Machine$double.eps, 1),
                         name = "Number of sites") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2025)) +
    theme(legend.title.position = "top")
  
  return(plot_i)
  
}

# 6. Map over the function ----

plot_year_dataset(territory_i = "Jamaica")
