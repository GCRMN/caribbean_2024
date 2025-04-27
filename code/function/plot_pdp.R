plot_pdp <- function(category_i){
  
  data_pdp %>% 
    filter(category == category_i) %>% 
    ggplot(data = .) +
    geom_ribbon(aes(x = x, ymin = lower_ci, ymax = upper_ci, fill = color), alpha = 0.5) +
    scale_fill_identity() +
    geom_line(aes(x = x, y = mean), color = "black") +
    scale_color_identity() +
    facet_wrap(~predictor, scales = "free", ncol = 4) +
    theme(strip.background = element_rect(fill = NA, color = NA)) +
    labs(x = "Predictor's value", y = "Percentage cover")
  
  ggsave(paste0("figs/05_supp-mat/pdp_", str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         height = 18, width = 12)
  
}
