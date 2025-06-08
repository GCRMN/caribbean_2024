plot_trends <- function(area_i){
  
  data_benthic <- data_benthic %>%
    mutate(color = case_when(category == "Hard coral" ~ "#c44d56",
                             category == "Algae" ~ "#16a085",
                             category == "Other fauna" ~ "#714d69",
                             category == "Macroalgae" ~ "#03a678",
                             category == "Turf algae" ~ "#26a65b",
                             category == "Coralline algae" ~ "#C5987D",
                             category == "Acropora" ~ "#e08283",
                             category == "Orbicella" ~ "#c44d56",
                             category == "Porites" ~ "#a37c82"),
           text_title = case_when(category == "Hard coral" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Algae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Other fauna" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  
                                  category == "Coralline algae" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Macroalgae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Turf algae" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  
                                  category == "Acropora" ~ 
                                    glue("**A.***<span style='color:{color}'> {category}</span>*"),
                                  category == "Orbicella" ~ 
                                    glue("**B.***<span style='color:{color}'> {category}</span>*"),
                                  category == "Porites" ~ 
                                    glue("**C.***<span style='color:{color}'> {category}</span>*")))
  
  data_benthic_plot <- data_benthic %>%
    group_by(year, area, category, color, text_title) %>% 
    summarise(mean = mean(measurementValue),
              sd = sd(measurementValue)) %>% 
    ungroup() %>% 
    mutate(ymin = mean - sd,
           ymin = ifelse(ymin < 0, 0, ymin),
           ymax = mean + sd) %>% 
    bind_rows(., data_benthic %>% 
                group_by(year, category, color, text_title) %>% 
                summarise(mean = mean(measurementValue),
                          sd = sd(measurementValue)) %>% 
                ungroup() %>% 
                mutate(ymin = mean - sd,
                       ymin = ifelse(ymin < 0, 0, ymin),
                       ymax = mean + sd,
                       area = "All")) %>% 
    complete(year, category, nesting(area), fill = list(mean = NA, sd = NA)) %>% 
    select(-color, -text_title) %>% 
    left_join(., data_benthic %>% 
                select(category, text_title, color) %>% 
                distinct()) %>% 
    drop_na(area) %>% 
    filter(area == area_i)
  
  data_trends_plot <- data_trends$smoothed_trends %>% 
    mutate(color = case_when(category == "Hard coral" ~ "#c44d56",
                             category == "Algae" ~ "#16a085",
                             category == "Other fauna" ~ "#714d69",
                             category == "Macroalgae" ~ "#03a678",
                             category == "Turf algae" ~ "#26a65b",
                             category == "Coralline algae" ~ "#C5987D",
                             category == "Acropora" ~ "#e08283",
                             category == "Orbicella" ~ "#c44d56",
                             category == "Porites" ~ "#a37c82"),
           text_title = case_when(category == "Hard coral" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Algae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Other fauna" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  
                                  category == "Coralline algae" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Macroalgae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Turf algae" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  
                                  category == "Acropora" ~ 
                                    glue("**A.***<span style='color:{color}'> {category}</span>*"),
                                  category == "Orbicella" ~ 
                                    glue("**B.***<span style='color:{color}'> {category}</span>*"),
                                  category == "Porites" ~ 
                                    glue("**C.***<span style='color:{color}'> {category}</span>*"))) %>% 
    filter(area == area_i)
  
  if(area_i == "All"){
    
    plot_i <- ggplot() +
      geom_pointrange(data = data_benthic_plot %>% 
                        filter(category %in% c("Hard coral", "Algae", "Other fauna")),
                      aes(x = year, y = mean, ymin = ymin, ymax = ymax),
                      show.legend = FALSE, size = 0.25, color = "grey") +
      geom_ribbon(data = data_trends_plot %>% 
                    filter(category %in% c("Hard coral", "Algae", "Other fauna")),
                  aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(data = data_trends_plot %>% 
                  filter(category %in% c("Hard coral", "Algae", "Other fauna")),
                aes(x = year, y = mean, color = color),
                linewidth = 1, show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_identity() +
      facet_wrap(~text_title) +
      theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
            strip.background = element_blank()) +
      labs(x = "Year", y = "Cover (%)") +
      lims(y = c(0, NA))
    
    ggsave(filename = "figs/01_part-1/fig-13.png", plot = plot_i, height = 5.5, width = 14, dpi = fig_resolution)
    
    plot_i <- ggplot() +
      geom_pointrange(data = data_benthic_plot %>% 
                        filter(category %in% c("Coralline algae", "Macroalgae", "Turf algae")),
                      aes(x = year, y = mean, ymin = ymin, ymax = ymax),
                      show.legend = FALSE, size = 0.25, color = "grey") +
      geom_ribbon(data = data_trends_plot %>% 
                    filter(category %in% c("Coralline algae", "Macroalgae", "Turf algae")),
                  aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(data = data_trends_plot %>% 
                  filter(category %in% c("Coralline algae", "Macroalgae", "Turf algae")),
                aes(x = year, y = mean, color = color),
                linewidth = 1, show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_identity() +
      facet_wrap(~text_title) +
      theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
            strip.background = element_blank()) +
      labs(x = "Year", y = "Cover (%)") +
      lims(y = c(0, NA))
    
    ggsave(filename = "figs/01_part-1/fig-14.png", plot = plot_i, height = 5.5, width = 14, dpi = fig_resolution)  
    
    plot_i <- ggplot() +
      geom_pointrange(data = data_benthic_plot %>% 
                        filter(category %in% c("Acropora", "Orbicella", "Porites")),
                      aes(x = year, y = mean, ymin = ymin, ymax = ymax),
                      show.legend = FALSE, size = 0.25, color = "grey") +
      geom_ribbon(data = data_trends_plot %>% 
                    filter(category %in% c("Acropora", "Orbicella", "Porites")),
                  aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(data = data_trends_plot %>% 
                  filter(category %in% c("Acropora", "Orbicella", "Porites")),
                aes(x = year, y = mean, color = color),
                linewidth = 1, show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_identity() +
      facet_wrap(~text_title) +
      theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
            strip.background = element_blank()) +
      labs(x = "Year", y = "Cover (%)") +
      lims(y = c(0, NA))
    
    ggsave(filename = "figs/01_part-1/fig-15.png", plot = plot_i, height = 5.5, width = 14, dpi = fig_resolution)  
    
  }else{
    
    plot_i <- ggplot() +
      geom_pointrange(data = data_benthic_plot %>% 
                        filter(category %in% c("Hard coral", "Macroalgae", "Other fauna")),
                      aes(x = year, y = mean, ymin = ymin, ymax = ymax),
                      show.legend = FALSE, size = 0.25, color = "grey") +
      geom_ribbon(data = data_trends_plot %>% 
                    filter(category %in% c("Hard coral", "Macroalgae", "Other fauna")),
                  aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(data = data_trends_plot %>% 
                  filter(category %in% c("Hard coral", "Macroalgae", "Other fauna")),
                aes(x = year, y = mean, color = color),
                linewidth = 1, show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_identity() +
      facet_wrap(~text_title) +
      theme(strip.text = element_markdown(hjust = 0, size = rel(1.3)),
            strip.background = element_blank()) +
      labs(x = "Year", y = "Cover (%)") +
      lims(y = c(0, NA))
    
    ggsave(filename = paste0("figs/02_part-2/fig-5/",
                             str_replace_all(str_replace_all(str_to_lower(area_i), " ", "-"), "---", "-"), ".png"),
           plot = plot_i, height = 5.5, width = 14, dpi = fig_resolution)
    
  }   
}
