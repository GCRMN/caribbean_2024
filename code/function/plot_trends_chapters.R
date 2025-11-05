plot_trends_chapters <- function(area_i, icons = FALSE, raw_data = TRUE,
                                 modelled_data = TRUE, reefcheck = TRUE, scales = "fixed"){
  
  # Filter data
  
  data_trends_i <- data_trends$raw_trends %>% 
    filter(area == area_i) %>% 
    filter(category %in% c("Hard coral", "Macroalgae"))
  
  if(reefcheck == TRUE){
    
    data_benthic_raw_i <- data_benthic_raw %>% 
      filter(area == area_i) %>% 
      filter(category %in% c("Hard coral", "Macroalgae"))
    
  }else{
    
    data_benthic_raw_i <- data_benthic_raw %>% 
      filter(area == area_i) %>% 
      filter(datasetID != "0015") %>%
      filter(category %in% c("Hard coral", "Macroalgae"))
    
  }
  
  if(reefcheck == TRUE){
    
    data_benthic_raw_mean_i <- data_benthic_raw %>% 
      filter(area == area_i) %>% 
      filter(category %in% c("Hard coral", "Macroalgae")) %>% 
      group_by(year, color, category, text_title) %>% 
      summarise(mean = mean(measurementValue)) %>% 
      ungroup() %>% 
      # Add row to show the subplot, even if empty
      add_row(text_title = "**B.**<span style='color:#03a678'> Macroalgae</span>")
    
  }else{
    
    data_benthic_raw_mean_i <- data_benthic_raw %>% 
      filter(area == area_i) %>% 
      filter(!(datasetID == "0015" & category == "Macroalgae" & measurementValue == 0)) %>%
      filter(category %in% c("Hard coral", "Macroalgae")) %>% 
      group_by(year, color, category, text_title) %>% 
      summarise(mean = mean(measurementValue)) %>% 
      ungroup() %>% 
      # Add row to show the subplot, even if empty
      add_row(text_title = "**B.**<span style='color:#03a678'> Macroalgae</span>")
    
  }
  
  # Base plot
  
  plot_trends <- ggplot() +
    {if(raw_data == TRUE & modelled_data == TRUE)
      geom_point(data = data_benthic_raw_mean_i,
                 aes(x = year, y = mean, color = "#b2bec3"), size = 1)} +
    {if(raw_data == TRUE & modelled_data == FALSE)
      geom_point(data = data_benthic_raw_i,
                 aes(x = year, y = measurementValue, color = "#b2bec3"), size = 0.8, alpha = 0.7)} +
    {if(raw_data == TRUE & modelled_data == FALSE)
      geom_point(data = data_benthic_raw_mean_i,
                 aes(x = year, y = mean, fill = color), color = "black", shape = 23, size = 2)} +
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
  
  # Icons
  
  if(icons == FALSE){
    
    plot_results <- plot_trends
    
  }else if(icons == TRUE){
    
    require(cowplot)
    
    data_icons <- tibble(category = c("Hard coral",
                                      "Macroalgae"),
                         path = c("figs/00_misc/icon_coral.png",
                                  "figs/00_misc/icon_macroalgae.png"))
    
    plot_results <- ggdraw(plot_trends) + 
      draw_image(as.character(data_icons[1, 2]),
                 x = 0.5285, y = 0.95, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.11, height = 0.11) +
      draw_image(as.character(data_icons[2, 2]),
                 x = 1.005, y = 0.95, # Position above right
                 hjust = 1, vjust = 1,
                 width = 0.11, height = 0.11) # Relative proportion of the image
    
  }else{
    
    stop("icons must be TRUE or FALSE")
    
  }
  
  # Save plots
  
  if(modelled_data == TRUE){
    
    ggsave(plot = plot_results, filename = paste0("figs/02_part-2/fig-5/",
                                                  str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                  "---", "-"),
                                                  ".png"),
           width = 12, height = 5)
    
  }else if(modelled_data == FALSE & reefcheck == TRUE){
    
    ggsave(plot = plot_results, filename = paste0("figs/02_part-2/fig-5b/",
                                                  str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                  "---", "-"),
                                                  ".png"),
           width = 12, height = 5)
    
  }else if(modelled_data == FALSE & reefcheck == FALSE){
    
    ggsave(plot = plot_results, filename = paste0("figs/02_part-2/fig-5c/",
                                                  str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"),
                                                                  "---", "-"),
                                                  ".png"),
           width = 12, height = 5)
    
     }else{
    
    stop("modelled_data must be TRUE or FALSE")
    
  }
  
}
