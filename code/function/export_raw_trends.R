export_raw_trends <- function(area_i){
  
  data_benthic_i <- data_benthic %>% 
    filter(area == area_i)
  
  wrap_plots(map(c("Hard coral", "Algae", "Other fauna"),
                 ~plot_raw_trends(., data_benthic_i = data_benthic_i)),
             axis_titles = "collect_y")
  
  ggsave(paste0("figs/06_additional/raw-data_",
                str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), "_category.png"),
         height = 4, width = 12)
  
  wrap_plots(map(c("Coralline algae", "Macroalgae", "Turf algae"),
                 ~plot_raw_trends(., data_benthic_i = data_benthic_i)),
             axis_titles = "collect_y")
  
  ggsave(paste0("figs/06_additional/raw-data_",
                str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), "_subcategory.png"),
         height = 4, width = 12)
  
  wrap_plots(map(c("Acropora", "Orbicella", "Porites"),
                 ~plot_raw_trends(., data_benthic_i = data_benthic_i)),
             axis_titles = "collect_y")
  
  ggsave(paste0("figs/06_additional/raw-data_",
                str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), "_genera.png"),
         height = 4, width = 12)
  
}
