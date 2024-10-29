# 1. Load packages ----

library(tidyverse)
library(readxl)
library(ggtext)

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/data_descriptors.R")

# 3. Select benthic data ----

load("data/02_misc/data-benthic.RData")

# 4. Transform data ----

data_sources <- read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  select(datasetID, rightsHolder) %>% 
  distinct()

data_year_dataset <- data_benthic %>% 
  group_by(datasetID, territory, year) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  select(datasetID, territory, year, nb_sites) %>% 
  complete(nesting(datasetID, territory),
           year = 1980:2024,
           fill = list(nb_sites = 0)) %>% 
  left_join(., data_sources) %>% 
  mutate(label = paste0("<b>", datasetID,
                        "</b><br><span style = 'font-size:10pt'>(",
                        rightsHolder, ")</span>"))

# 5. Create a function to produce the plot ----

plot_year_dataset <- function(territory_i){
  
  data_year_dataset_i <- data_year_dataset %>% 
    filter(territory == territory_i)
  
  nb_datasets_i <- length(unique(data_year_dataset_i$datasetID))
  
  plot_i <- ggplot(data = data_year_dataset_i,
                   aes(x = year, y = label, fill = nb_sites)) +
    geom_tile(color = "white", height = 0.6, linewidth = 0.6) +
    theme_graph() +
    labs(y = NULL, x = "Year") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(expand = c(0, 0), limits = c(1979, 2025)) +
    theme(legend.title.position = "top",
          legend.title = element_text(size = 10, hjust = 1, face = "bold", color = "#2c3e50"),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.justification = "right",
          axis.text.y = element_markdown())
  
  if(max(data_year_dataset_i$nb_sites) == 1){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, 2),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }else if(max(data_year_dataset_i$nb_sites) == 2){
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, 1, 2),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, 1, 2)),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
   }else if(max(data_year_dataset_i$nb_sites) == 3){
      
      plot_i <- plot_i +
        scale_fill_stepsn(breaks = c(0, 1, 2, 3),
                          colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[4]),
                          limits = c(0, max(data_year_dataset_i$nb_sites)),
                          values = scales::rescale(c(0, 1, 2, 3)),
                          labels = scales::label_number(accuracy = 1),
                          show.limits = TRUE,
                          right = FALSE,
                          name = "NUMBER OF SITES")
    
  }else{
    
    plot_i <- plot_i +
      scale_fill_stepsn(breaks = c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0)),
                        colors = c("lightgrey", "lightgrey", palette_second[2], palette_second[2], palette_second[3],
                                   palette_second[4], palette_second[5]),
                        limits = c(0, max(data_year_dataset_i$nb_sites)),
                        values = scales::rescale(c(0, round(seq(1, max(data_year_dataset_i$nb_sites), length.out = 6), 0))),
                        labels = scales::label_number(accuracy = 1),
                        show.limits = TRUE,
                        right = FALSE,
                        name = "NUMBER OF SITES")
    
  }
  
  ggsave(filename = paste0("figs/02_part-2/fig-4/",
                           str_replace_all(str_replace_all(str_to_lower(territory_i), " ", "-"), "---", "-"), ".png"),
         plot = plot_i, height = (2 + (3*0.3*nb_datasets_i)), width = 9, dpi = fig_resolution)
  
}

# 6. Map over the function ----

map(unique(data_benthic$territory), ~plot_year_dataset(territory_i = .))
