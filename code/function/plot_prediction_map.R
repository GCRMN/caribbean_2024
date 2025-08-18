plot_prediction_map <- function(category_i){
  
  plot_maps <- ggplot() +
    geom_sf(data = data_predicted %>% filter(category == category_i),
            aes(fill = cover_pred, color = cover_pred)) +
    scale_fill_gradientn(colours = c("#89c4f4", "#2c82c9", "#f1d693", "#fbc093", "#c44d56", "#af4154", "#5a228b"),
                         limits = c(0, NA),
                         name = paste0("Predicted\n", str_to_lower(category_i), "\ncover")) +
    scale_color_gradientn(colours = c("#89c4f4", "#2c82c9", "#f1d693", "#fbc093", "#c44d56", "#af4154", "#5a228b"),
                          limits = c(0, NA),
                          name = paste0("Predicted\n", str_to_lower(category_i), "\ncover")) +
    facet_wrap(~time_period, ncol = 3) +
    geom_sf(data = data_land_ne, linewidth = 0.1) +
    theme(strip.background = element_rect(fill = NA, linewidth = 0),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(fill = NA),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 12),
          panel.grid = element_blank()) +
    coord_sf(xlim = c(-105, -50), ylim = c(6, 38), expand = FALSE)
  
  ggsave(paste0("figs/06_additional/03_model-evaluation/pred-map_",
                str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         plot = plot_maps, width = 11, height = 3.5)
  
}
