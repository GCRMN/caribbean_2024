plot_trends <- function(area_i, icons, raw_data = TRUE, modelled_data = TRUE, scales = "fixed"){
  
  data_trends_i <- data_trends$smoothed_trends %>% 
    filter(area == area_i) %>% 
    filter(category %in% c("Hard coral", "Macroalgae", "Other fauna"))
  
  data_raw_i <- data_benthic %>% 
    filter(area == area_i) %>% 
    filter(category %in% c("Hard coral", "Macroalgae", "Other fauna"))
  
  plot_trends <- ggplot() +
    {if(raw_data == TRUE)
      geom_pointrange(data = data_raw_i,
                      aes(x = year, y = mean, ymin = ymin,
                          ymax = ymax, color = color), size = 0.25)} +
    {if(modelled_data == TRUE)
      geom_ribbon(data = data_trends_i,
                  aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95,
                      fill = color), alpha = 0.5, show.legend = FALSE)} +
    {if(modelled_data == TRUE)
      geom_line(data = data_trends_i,
                aes(x = year, y = mean, color = color),
                linewidth = 1, show.legend = FALSE)} +
    scale_color_identity() +
    scale_fill_identity() +
    facet_wrap(~text_title, scales = scales) +
    theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
          strip.background = element_blank(),
          panel.spacing = unit(2, "lines")) +
    labs(x = "Year", y = "Cover (%)") +
    lims(x = c(1980, 2024), y = c(0, NA))
  
  if(icons == FALSE){
    
    if(area_i == "All"){
      
      ggsave(plot = plot_trends, filename = "figs/01_part-1/fig-12.png", width = 14, height = 5)
      
    }else{
      
      ggsave(plot = plot_trends, filename = paste0("figs/02_part-2/fig-5/",
                                                   str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                   "---", "-"),
                                                   ".png"),
             width = 14, height = 5)
      
    }
    
  }else if(icons == TRUE){
    
    require(cowplot)
    
    logo_coral <- "figs/00_misc/icons_acropora-palmata.png"
    logo_macroalgae <- "figs/00_misc/icons_macroalgae.png"
    logo_fauna <- "figs/00_misc/icons_gorgonia.png"
    
    plot_trends <- ggdraw(plot_trends) + 
      draw_image(logo_coral,
                 x = 0.39, y = 0.96, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.15, height = 0.15) +
      draw_image(logo_macroalgae,
                 x = 0.7075, y = 0.96, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.15, height = 0.15) + # Relative proportion of the image
      draw_image(logo_fauna,
                 x = 1.0175, y = 0.96, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.15, height = 0.15) # Relative proportion of the image
    
    if(area_i == "All"){
      
      ggsave(plot = plot_trends, filename = "figs/01_part-1/fig-12.png", width = 14, height = 5)
      
    }else{
      
      ggsave(plot = plot_trends, filename = paste0("figs/02_part-2/fig-5/",
                                                   str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                   "---", "-"),
                                                   ".png"),
             width = 14, height = 5)
      
    }
    
  }else{
    
    stop("icons must be TRUE or FALSE")
    
  }
  
}
