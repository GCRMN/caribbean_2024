plot_raw_trends <- function(category_i, data_benthic_i = data_benthic_i){
  
  data_benthic_j <- data_benthic_i %>% 
    filter(category == category_i)
  
  ggplot(data = data_benthic_j, aes(x = year, y = mean, color = color)) +
    geom_pointrange(aes(y = mean, ymin = ymin, ymax = ymax, color = color), size = 0.25) +
    scale_color_identity() +
    theme_graph() +
    lims(y = c(0, NA), x = c(1980, 2024)) +
    labs(x = "Year", y = "Cover (%)", title = unique(data_benthic_j$text_title)) +
    theme(plot.title = element_markdown())
  
}
