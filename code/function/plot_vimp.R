plot_vimp <- function(category_i){
  
  data_imp_summary_i <- data_imp_summary %>% 
    filter(category == category_i)
  
  data_imp_raw_i <- data_imp_raw %>% 
    filter(category == category_i)
  
  plot_i <- ggplot(data = data_imp_summary_i, aes(x = fct_reorder(predictor, mean), color = color)) +
    geom_jitter(data = data_imp_raw_i,
                aes(x = fct_reorder(predictor, mean), y = importance, color = color),
                alpha = 0.3, width = 0.15) +
    geom_linerange(aes(ymin = lower_ci_95, ymax = upper_ci_95), linewidth = 1) +
    geom_linerange(aes(ymin = lower_ci_80, ymax = upper_ci_80), linewidth = 1.5) +
    geom_point(aes(y = mean), size = 3.5, shape = 21, fill = "white") +
    coord_flip() +
    scale_color_identity() +
    labs(x = NULL, y = "Relative importance (%)") +
    lims(y = c(0, NA))
  
  ggsave(filename = paste0("figs/05_supp-mat/supp_fig_4_", str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         plot = plot_i, height = 8, width = 6, dpi = fig_resolution)
  
}
