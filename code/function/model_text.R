model_text <- function(category_i){
  
  category_name <- case_when(category_i %in% c("Acropora", "Orbicella", "Porites") ~ category_i,
                             TRUE ~ str_to_lower(category_i))
  
  value_training <- model_results$model_description %>% 
    filter(category == category_i) %>% 
    select(nb_training) %>% 
    pull()
  
  value_rmse <- readxl::read_xlsx("figs/06_additional/03_model-evaluation/performance_rmse.xlsx") %>% 
    filter(area == "Entire region") %>% 
    select(all_of(category_i)) %>% 
    pull()
  
  value_rsq <- readxl::read_xlsx("figs/06_additional/03_model-evaluation/performance_rsq.xlsx") %>% 
    filter(area == "Entire region") %>% 
    select(all_of(category_i)) %>% 
    pull()
  
  value_trend <- data_trends$long_term_trend %>% 
    filter(category == category_i) %>% 
    filter(area == "All") %>%
    mutate(p_value = format(p_value, digits = 3))
  
  values_mean <- data_trends$raw_trends %>%
    filter(category == category_i) %>% 
    filter(area == "All") %>% 
    summarise(across(c(mean, lower_ci_95, upper_ci_95), ~round(mean(.x, na.rm = TRUE), 1)))
  
  values_comparison <- data_trends$raw_trends %>%
    filter(category == category_i) %>% 
    filter(area == "All") %>% 
    mutate(period = case_when(year %in% c(1980:1985) ~ "1980-1985",
                              year %in% c(2019:2024) ~ "2019-2024",
                              TRUE ~ NA)) %>% 
    drop_na(period) %>% 
    group_by(period) %>% 
    summarise(across(c(mean, lower_ci_95, upper_ci_95), ~round(mean(.x, na.rm = TRUE), 1))) %>% 
    ungroup()
  
  values_1980 <- values_comparison %>% 
    filter(period == "1980-1985")
    
  values_2019 <- values_comparison %>% 
    filter(period == "2019-2024")
    
  value_diff <- pull(abs(values_comparison[2, "mean"] - values_comparison[1, "mean"]))
  
  result <- paste0("The temporal trend of benthic cover of ", category_name,
                   " across Caribbean coral reefs from 1980 to 2024 (Figure 1.3.XX) was estimated using data ",
                   "from X monitoring sites and ", format(value_training, big.mark = ","), " observations employed for model training. ",
                   "The model achieved a Root Mean Squared Error (RMSE) of ", value_rmse,
                   " and a coefficient of determination (R²) of ", value_rsq, ".",
                   " These performance metrics indicate that the model's predictions deviate on average by ",
                   value_rmse, " in percentage cover from the observed values, while explaining ",
                   value_rsq*100, "% of the variance in the data.",
                   " From 1980 to 2024, the long-term average in the modeled benthic cover of ", category_name, " across coral reefs",
                   " of the Caribbean was ", pull(values_mean[1,"mean"]), "% (95% CI [", pull(values_mean[1,"lower_ci_95"]),
                   "% - ", pull(values_mean[1,"upper_ci_95"]), "%]). Over this period, ")
  
  if(value_trend$trend == "Negative trend"){
    
    result <- paste0(result, "the long-term trend in ", category_name, " cover exhibited a significant",
                     " decline (Mann-Kendall test, τ = ", round(as.numeric(value_trend$tau), 2),", p-value = ",
                     value_trend$p_value,"), decreasing",
                     " from an average of ", pull(values_1980[1,"mean"]), "% (95% CI [", pull(values_1980[1,"lower_ci_95"]),
                     "% - ", pull(values_1980[1,"upper_ci_95"]), "%]) in 1980–1985 to ",pull(values_2019[1,"mean"]),
                     "% (95% CI [", pull(values_2019[1,"lower_ci_95"]),
                     "% - ", pull(values_2019[1,"upper_ci_95"]), "%]) in 2019–2024.",
                     " This corresponds to an absolute decline in ",
                     value_diff,
                     "% ", category_name, " cover between the two periods.")
    
  }else if(value_trend$trend == "No trend"){
    
    result <- paste0(result, "the benthic cover of ", category_name, " remained relatively stable",
                     " (Mann-Kendall test, τ = ", round(as.numeric(value_trend$tau), 2), ", p-value = ",
                     value_trend$p_value, ").")
    
  }else if(value_trend$trend == "Positive trend"){
    
    result <- paste0(result, "the long-term trend in ", category_name, " cover exhibited a significant increase",
                     " (Mann-Kendall test, τ = ", round(as.numeric(value_trend$tau), 2), ", p-value = ",
                     value_trend$p_value,"), rising",
                     " from an average of ", pull(values_1980[1,"mean"]), "% (95% CI [", pull(values_1980[1,"lower_ci_95"]),
                     "% - ", pull(values_1980[1,"upper_ci_95"]), "%]) in 1980–1985 to ",pull(values_2019[1,"mean"]),
                     "% (95% CI [", pull(values_2019[1,"lower_ci_95"]),
                     "% - ", pull(values_2019[1,"upper_ci_95"]), "%]) in 2019–2024.",
                     " This corresponds to an absolute increase in ",
                     value_diff,
                     "% ", category_name, " cover between the two periods.")
    
  }else{
    
    stop("Values can only take Negative trend, Positive trend, or No trend")
    
  }
  
  print(result)
  
}
