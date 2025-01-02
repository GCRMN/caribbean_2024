combine_plot_trends <- function(area_i, categ_type){
  
  if(categ_type == "categories"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(area == area_i) %>% 
      filter(category %in% c("Hard coral", "Macroalgae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Hard coral", "Macroalgae"),
             category = fct_relevel(category, "Hard coral", "Macroalgae"))
    
    plot_list <- map(levels(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon"))
    
    plot_i <- wrap_plots(plot_list, ncol = 1)
    
    if(area_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-13.png", plot = plot_i, height = 12, width = 5, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-5/", str_replace_all(str_to_lower(area_i), " ", "-"), ".png"),
             plot = plot_i, height = 12, width = 5, dpi = fig_resolution)
      
    }
    
  }else if(categ_type == "families"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(area == area_i) %>% 
      filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Acroporidae", "Pocilloporidae", "Poritidae"),
             category = fct_relevel(category, "Acroporidae", "Pocilloporidae", "Poritidae"))
    
    plot_list <- map(levels(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon"))
    
    plot_i <- wrap_plots(plot_list, nrow = 1)
    
    if(area_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-14.png", plot = plot_i, height = 4, width = 12, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-7/", str_replace_all(str_to_lower(area_i), " ", "-"), ".png"),
             plot = plot_i, height = 4, width = 12, dpi = fig_resolution)
      
    }
    
  }else{
    
    stop("categ_type argument can only take 'categories' or 'families'")
    
  }
}
