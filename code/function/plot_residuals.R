plot_residuals <- function(category_i, all = FALSE){
  
  if(all == TRUE){
    
    plot_i <- model_results$model_pred_obs %>%
      filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae", "Other fauna")) %>% 
      mutate(residual = yhat - y) %>% 
      ggplot(data = ., aes(x = residual, fill = color)) + 
      geom_histogram(aes(y = after_stat(count / sum(count))*100),
                     alpha = 0.5) +
      geom_vline(xintercept = 0) +
      scale_fill_identity() +
      facet_wrap(~text_title, scales = "free") +
      lims(x = c(-100, 100)) +
      labs(x = "Residual (ŷ - y)", y = "Percentage") +
      theme(strip.text = element_markdown(hjust = 0),
            strip.background = element_blank())
    
    ggsave(plot_i, filename = "figs/05_supp-mat/03_distri-residuals_all_cat.png",
           dpi = fig_resolution, height = 6, width = 8)
    
  }else{
    
    plot_i <- model_results$model_pred_obs %>%
      mutate(residual = yhat - y) %>%
      filter(category == category_i) %>% 
      ggplot(data = ., aes(x = residual, fill = color)) + 
        geom_histogram(aes(y = after_stat(count / sum(count))*100),
                       alpha = 0.5) +
        geom_vline(xintercept = 0) +
        scale_fill_identity() +
        facet_wrap(~territory, scales = "free", ncol = 5) +
        lims(x = c(-100, 100)) +
        labs(x = "Residual (ŷ - y)", y = "Percentage") +
        theme(strip.text = element_markdown(hjust = 0.5),
              strip.background = element_blank())
    
    ggsave(plot_i, filename = paste0("figs/05_supp-mat/03_distri-residuals_", 
                                     str_replace_all(str_to_lower(category_i), " ", "-"),
                                     ".png"),
           width = 15, height = 12, dpi = fig_resolution)
    
  }
  
}
