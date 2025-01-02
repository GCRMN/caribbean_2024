plot_pred_obs <- function(category_i, all = FALSE){
  
  if(all == TRUE){
    
    plot_i <- model_results$model_pred_obs %>%
      filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
      ggplot(data = ., aes(x = y, y = yhat, color= color)) +
      geom_point(alpha = 0.1) +
      geom_abline(slope = 1, linewidth = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
      scale_color_identity() +
      facet_wrap(~text_title, scales = "free") +
      labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
      theme(strip.text = element_markdown(hjust = 0),
            strip.background = element_blank())
    
    ggsave(plot_i, filename = "figs/05_supp-mat/02_pred-vs-obs_all_cat.png",
           dpi = fig_resolution, height = 6, width = 8)
    
    plot_i <- model_results$model_pred_obs %>%
      filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
      ggplot(data = ., aes(x = y, y = yhat, color= color)) +
      geom_point(alpha = 0.1) +
      geom_abline(slope = 1, linewidth = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
      scale_color_identity() +
      facet_wrap(~text_title, scales = "free") +
      labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
      theme(strip.text = element_markdown(hjust = 0),
            strip.background = element_blank())
    
    ggsave(plot_i, filename = "figs/05_supp-mat/02_pred-vs-obs_all_fam.png",
           dpi = fig_resolution, height = 4, width = 10)
    
  }else{
    
    data_i <- model_results$model_pred_obs %>%
      filter(category == category_i)
    
    plot_i <- ggplot(data = data_i, aes(x = y, y = yhat, color= color)) +
      geom_point(alpha = 0.2) +
      geom_abline(slope = 1, linewidth = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
      scale_color_identity() +
      facet_wrap(~territory, scales = "free", ncol = 5) +
      labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
      theme(strip.text = element_markdown(hjust = 0.5),
            strip.background = element_blank())
    
    ggsave(plot_i, filename = paste0("figs/05_supp-mat/02_pred-vs-obs_", 
                                     str_replace_all(str_to_lower(category_i), " ", "-"),
                                     ".png"),
           width = 15, height = 12, dpi = fig_resolution)
    
  }
}
