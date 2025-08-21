model_text <- function(category_i){
  
  category_name <- case_when(category_i %in% c("Acropora", "Orbicella", "Porites") ~ category_i,
                             TRUE ~ str_to_lower(category_i))
  
  value_training <- model_results$model_description %>% 
    filter(category == category_i) %>% 
    select(nb_training) %>% 
    pull()
  
  value_rmse <- readxl::read_xlsx("figs/06_additional/03_model-evaluation/performance_rmse.xlsx") %>% 
    filter(area == "Entire region") %>% 
    select(category_i) %>% 
    pull()
  
  value_rsq <- readxl::read_xlsx("figs/06_additional/03_model-evaluation/performance_rsq.xlsx") %>% 
    filter(area == "Entire region") %>% 
    select(category_i) %>% 
    pull()
  
  value_trend <- data_trends$long_term_trend %>% 
    filter(category == category_i) %>% 
    filter(area == "All")
  
  values_mean <- data_trends$smoothed_trends %>%
    filter(category == category_i) %>% 
    filter(area == "All") %>% 
    summarise(across(c(mean, lower_ci_95, upper_ci_95), ~round(mean(.x, na.rm = TRUE), 1)))
  
  result <- paste0("The temporal trends of benthic cover of ", category_name,
                   " across Caribbean coral reefs from 1980 to 2024 were estimated using data ",
                   "from X monitoring sites and ", format(value_training, big.mark = ","), " observations employed for model training (Supp Table X). ",
                   "Model evaluation metrics indicated good predictive performance, with a Root Mean Square ",
                   "Error (RMSE) of ", value_rmse, " and a coefficient of determination (R²) of ", value_rsq, ".",
                   " From 1980 to 2024, the long-term average in the modeled benthic cover of ", category_name, " across coral reefs",
                   " of the Caribbean was ", pull(values_mean[1,"mean"]), "% (95% CI [", pull(values_mean[1,"lower_ci_95"]),
                   "% - ", pull(values_mean[1,"upper_ci_95"]), "%]). Over this period, ")
  
  if(value_trend$trend == "Negative trend"){
    
    result <- paste0(result, "the long-term trend in ", category_name, " benthic cover exhibited a significant",
                     " decline (Mann-Kendall test, τ = ", round(as.numeric(value_trend$tau), 2),", p-value = ",
                     1,") decreasing",
                     " from an average of X% in 1980–1985 to X% in 2019–2024.",
                     " This corresponds to an average absolute annual decline of X%.")
    
    
  }else if(value_trend$trend == "No trend"){
    
    result <- paste0(result, "the benthic cover of ", category_name, " remained relatively stable",
                     " (Mann-Kendall test, τ = ", round(as.numeric(value_trend$tau), 2), ", p-value = ",
                     1, ").")
    
    
  }else if(value_trend$trend == "Positive trend"){
    
    result <- paste0(result, "the long-term trend in ", category_name, " benthic cover exhibited a significant increase",
                     " (Mann-Kendall test, τ = ", round(as.numeric(value_trend$tau), 2), ", p-value = ",
                     1,"), rising from an average of X%",
                     " in 1980–1985 to X% in 2019–2024. This corresponds to an average",
                     " absolute annual increase of X%.")
    
  }else{
    
    stop("Values can only take Negative trend, Positive trend, or No trend")
    
  }
  
  print(result)
  
}
