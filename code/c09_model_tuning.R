# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(sf)
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)
library(future)
library(furrr)

options(future.globals.maxSize = 100000*1024^2) # 100 Gb
plan(strategy = multisession, workers = 4)

# 2. Load data ----

load("data/11_model-data/data_benthic_prepared.RData")
load("data/11_model-data/data_predictors_pred.RData")

# 3. Create the function ----

hyperparam_tuning <- function(category_i){
  
  # 1. Data preparation
  
  ## 1.1 Filter the category
  
  data_split <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category)
  
  ## 1.2 Split into training and testing data
  
  data_split <- initial_split(data_split, prop = 3/4)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 2. Hyperparameters tuning
  
  ## 2.1 Define the recipe
  
  boosted_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    step_dummy(all_nominal_predictors())
  
  ## 2.2 Define the model
  
  boosted_model <- boost_tree(learn_rate = tune(),
                              trees = tune(), 
                              min_n = tune(), 
                              tree_depth = tune()) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 2.3 Define the workflow
  
  boosted_workflow <- workflow() %>%
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  ## 2.4 Create the grid
  
  tune_grid <- grid_max_entropy(learn_rate(),
                                trees(),
                                tree_depth(),
                                min_n(),
                                size = 30)
  
  ## 2.5 Run the hyperparameters tuning
  
  tuned_results <- tune_grid(boosted_workflow,
                             resamples = vfold_cv(data_train, v = 5),
                             grid = tune_grid)
  
  ## 2.6 Get best set of parameters
  
  model_hyperparams <- select_best(tuned_results, metric = "rmse") %>% 
    select(-".config") %>% 
    as_tibble(.) %>%
    mutate(nb_training = nrow(data_train),
           nb_testing = nrow(data_test)) %>% 
    mutate(category = category_i)
  
  # 3. Predicted vs observed
  
  ## 3.1 Redefine the model (with hyperparameters)
  
  boosted_model <- boost_tree(learn_rate = model_hyperparams$learn_rate,
                              trees = model_hyperparams$trees, 
                              min_n = model_hyperparams$min_n, 
                              tree_depth = model_hyperparams$tree_depth) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 3.2 Redefine the workflow
  
  boosted_workflow <- workflow() %>%
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  ## 3.3 Fit the final model
  
  final_model <- boosted_workflow %>%
    last_fit(data_split)
  
  final_fitted <- final_model$.workflow[[1]]
  
  ## 3.4 Model performance
  
  model_performance <- collect_metrics(final_model) %>% 
    select(-".estimator", -".config") %>% 
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    mutate(category = category_i)
  
  ## 3.4 Predicted vs Observed
  
  result_pred_obs <- data_test %>% 
    mutate(yhat = predict(final_fitted, data_test)$.pred) %>% 
    rename(y = measurementValue) %>% 
    select(territory, y, yhat) %>% 
    mutate(category = category_i)
  
  # 4. Return the results
  
  return(lst(model_hyperparams,
             model_performance,
             result_pred_obs))
  
}

# 4. Map over the function ----

tuning_results <- future_map(c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae",
                               "Acroporidae", "Poritidae", "Pocilloporidae"),
                             ~hyperparam_tuning(category_i = .)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

# 5. Export the data ----

save(tuning_results, file = "data/12_model-output/model_tuning.RData")
