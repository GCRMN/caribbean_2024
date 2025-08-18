plot_pdp <- function(category_i){
  
  data_pdp %>% 
    filter(category == category_i) %>% 
    ggplot(data = .) +
    geom_line(aes(x = x, y = y_pred, group = bootstrap), color = "lightgrey") +
    geom_smooth(aes(x = x, y = y_pred, color = color), method = "gam", level = 0.99) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_wrap(~predictor, scales = "free", ncol = 4) +
    theme(strip.background = element_rect(fill = NA, color = NA),
          strip.text = element_text(face = "bold")) +
    labs(x = "Predictor's value", y = "Percentage cover")
    
  ggsave(paste0("figs/06_additional/03_model-evaluation/pdp_", str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         height = 18, width = 12)
  
}
