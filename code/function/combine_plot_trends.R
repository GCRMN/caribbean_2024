combine_plot_trends <- function(area_i, categ_type){
  
  if(categ_type == "category"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(area == area_i) %>% 
      filter(category %in% c("Hard coral", "Algae", "Other fauna")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Hard coral", "Algae", "Other fauna"),
             category = fct_relevel(category, "Hard coral", "Algae", "Other fauna"))
    
    plot_list <- map(levels(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon"))
    
    plot_i <- wrap_plots(plot_list, nrow = 1)
    
    if(area_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-13.png", plot = plot_i, height = 4, width = 12, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-5/",
                               str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 4, width = 12, dpi = fig_resolution)
      
    }
    
  }else if(categ_type == "subcategory"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(area == area_i) %>% 
      filter(category %in% c("Coralline algae", "Macroalgae", "Turf algae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Coralline algae", "Macroalgae", "Turf algae"),
             category = fct_relevel(category, "Coralline algae", "Macroalgae", "Turf algae"))
    
    plot_list <- map(levels(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon"))
    
    plot_i <- wrap_plots(plot_list, nrow = 1)
    
    if(area_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-14.png", plot = plot_i, height = 4, width = 12, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 4, width = 12, dpi = fig_resolution)
      
    }
    
  }else if(categ_type == "genus"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(area == area_i) %>% 
      filter(category %in% c("Acropora", "Orbicella", "Porites")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Acropora", "Orbicella", "Porites"),
             category = fct_relevel(category, "Acropora", "Orbicella", "Porites"))
    
    plot_list <- map(levels(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon"))
    
    plot_i <- wrap_plots(plot_list, nrow = 1)
    
    if(area_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-15.png", plot = plot_i, height = 4, width = 12, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-7/",
                               str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 4, width = 12, dpi = fig_resolution)
      
    }
    
  }else{
    
    stop("categ_type argument can only take 'categories' or 'families'")
    
  }
}
