plot_trends <- function(category_i, data_trends_i, show_obs_data = "none"){
  
  data_trends_j <- data_trends_i %>% 
    filter(category == category_i)
  
  if(show_obs_data == "none"){
    
    plot_j <- ggplot(data = data_trends_j) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
      geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
      labs(x = NULL, y = "Cover (%)", title = unique(data_trends_j$text_title)) +
      theme(plot.title = element_markdown())
    
  }else if(show_obs_data == "rug"){
    
    plot_j <- ggplot(data = data_trends_j) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
      geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
      labs(x = NULL, y = "Cover (%)", title = unique(data_trends_j$text_title)) +
      theme(plot.title = element_markdown()) +
      geom_rug(data = data_trends_j %>% filter(data_obs != 0), aes(x = year), sides = "b")
    
  }else if(show_obs_data == "ribbon"){
    
    cm <- data_trends_j %>%
      mutate(rleid = with(rle(data_obs), rep(seq_along(lengths), lengths)),
             group = as.integer(rleid))
    
    cm1 <- cm %>% 
      ungroup %>% 
      mutate(d = 3) %>%
      uncount(d, .id = "A") %>%
      mutate_at(vars(year, mean, lower_ci_95, lower_ci_80, upper_ci_95, upper_ci_80),
                function(x=.) ifelse(.$A == 1,(x + lag(x))/2,
                                     ifelse(.$A == 3, (x + lead(x))/2, x))) %>%
      group_by_at(group_vars(cm)) %>%
      filter(row_number()!= 1, row_number() !=n()) %>% 
      ungroup() %>% 
      select(-A, -rleid)
    
    plot_j <- ggplot(data = cm1) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = as.factor(data_obs), group = group), 
                  alpha = 0.25, show.legend = FALSE) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = as.factor(data_obs), group = group), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(aes(x = year, y = mean, color = as.factor(data_obs), group = group), 
                linewidth = 1, show.legend = FALSE) +
      scale_fill_manual(breaks = c("0", "1"), values = c("grey", unique(data_trends_j$color))) +
      scale_color_manual(breaks = c("0", "1"), values = c("grey", unique(data_trends_j$color))) +
      scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
      labs(x = NULL, y = "Cover (%)", title = unique(data_trends_j$text_title)) +
      theme(plot.title = element_markdown())
    
  }else{
    
    stop("show_obs_data argument can only take 'none', 'rug', or 'ribbon'")
    
  }
  
  return(plot_j)
  
}
