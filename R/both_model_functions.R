library(skimr)
require(tidyverse)
require(modeltime)
require(tidymodels)
library(workflowsets)
require(timetk)
require(xgboost)
require(caret)
library(lubridate)
library(devtools)
library(plotly)
library(openxlsx)
library(writexl)
require(randomForest)


# 1. Create model functions -----------------------------------------------

split <- function(dataset, threshold){
  # This function splits the dataset, this assumes the dataset is in chronological order.
  # Receives a dataset and a threshold
  # Devolves a split_tidymodels object, to access the training just use training(model_split)
  model_split <- time_series_split(dataset,
                                   initial = floor(nrow(dataset)*threshold),
                                   assess = nrow(dataset)-floor(nrow(dataset)*threshold))
  return(model_split)
}

seed <- function(seed){
  # This function selects the replicable seed for using the model
  set.seed(seed)
}


# 2. Model Preprocessing --------------------------------------------------

set_recipe <- function(formula, split_object, threshold, drop){
  # This function realizes the preprocessing, it takes three arguments (for now)
  # KPI - The interest variable
  # split_object - The split of the dataset
  # threshold - threshold for correlation beetween variables
  # dropped - variables to be deleted from the model
  model_recipe <- recipe(formula, data = training(split_object)) %>% # Formula of the model
    step_nzv(all_predictors()) %>% # Deletes near zero variance predictors
    step_corr(all_numeric(),-all_outcomes(), threshold = threshold) %>% # Drops highly correlated variables
    step_select(all_numeric(), -contains("_val|_vol"))  # selects only numeric variables minus _Vol, _val

  return(model_recipe)
}

# 3. Model workflowset ----------------------------------------------------

set_model_wfset <- function(model_recipe, model){
  # This function sets the model workflow
  # model_recipe - uses the recipe made for the model
  # model - is the table created by the function rf_model_table_set

  model_wfset <- workflow_set(
    preproc = list(model_recipe),
    models = model$.models)
  return(model_wfset)
}

# 4. Fit the models -------------------------------------------------------

set_model_parallel_tbl <- function(dataset, model_wfset, split_object){
  # This functions generates the list of models
  # dataset - uses the original dataset
  # model_wfset - uses the output from set_model_wfset
  # split_object - uses the split object created

  model_parallel_tbl <- model_wfset %>%
    modeltime_fit_workflowset(
      data    = training(split_object),
      control = control_fit_workflowset(
        verbose   = TRUE,
        allow_par = TRUE
      )
    ) %>%
    modeltime_calibrate(training(split_object)) %>%
    modeltime_refit(data = dataset)
  return(model_parallel_tbl)
}

# 5. Get the base model ---------------------------------------------------

get_best_model <- function(model_parallel_tbl){
  # This function returns the ID of the best model
  # model_parallel_tbl - uses the table of models
  model_best_id <- as.numeric(model_parallel_tbl %>%
                                modeltime_accuracy() %>% arrange(desc(rsq)) %>%
                                dplyr::slice(1) %>% select(.model_id))
  return(model_best_id)
}

get_base_model <- function(model_best_id, model_parallel_tbl){
  # This function returns the model object to be used in posterior analysis
  # model_best_id - the id of the best model obtained
  # model_parallel_tbl - uses the table of models
  model_base <- model_parallel_tbl %>% filter(.model_id == model_best_id) %>%
    select(.model) %>%
    pluck(1,1) %>%
    chuck('fit') %>%
    chuck('fit') %>%
    chuck('fit')
  return(model_base)
}


# 6. Get the predictions --------------------------------------------------

get_model_predictions <- function(date_var, model_parallel_tbl, model_best_id, split_object, KPI){
  model_predictions <- tibble("Date" = date_var,
                            "Predicted" = c(model_parallel_tbl %>%
                                              modeltime_residuals() %>%
                                              filter(.model_id == model_best_id) %>%
                                              select(.prediction) %>% pull(),
                                            model_parallel_tbl %>%
                                              modeltime_forecast(new_data = testing(split_object)) %>%
                                              filter(.model_id == model_best_id) %>%
                                              select(.value) %>% pull()),
                            "Real" = KPI %>% pull()) %>%
    mutate(Residuals = Real - Predicted)
  return(model_predictions)
}

plot_model_predictions <- function(model_predictions){
  ggplotly(ggplot(model_predictions%>%
                    pivot_longer(cols = c('Predicted', 'Real', 'Residuals'),
                                 values_to = "Values",
                                 names_to = "Item") %>%
                    mutate(Date = unlist(Date),
                           Values = unlist(Values),
                           Item = unlist(Item))) +
             aes(x = Date, y = Values, colour = Item, group = Item) +
             geom_line(size = 1.05) +
             scale_color_manual(values = c(Predicted = "#440154", Real = "#EF0707", Residuals = "#E2D249")) +
             labs(y = "Volume Sold", title = "Actual vs Predicted") +
             theme_classic() +
             theme(plot.title = element_text(face = "bold",
                                             hjust = 0.5),
                   axis.title.y = element_text(size = 13L, face = "bold"),
                   axis.title.x = element_text(size = 13L, face = "bold")))
}


# 7. Error specific metrics -----------------------------------------------

get_model_metrics <- function(model_parallel_tbl, model_best_id, newdata){
  model_metrics <- model_parallel_tbl %>%
    modeltime_accuracy(new_data = newdata) %>%
    filter(.model_id == model_best_id)
}

get_error_metrics <- function(model_parallel_tbl, dataset, model_best_id){
  model_error_metrics <- model_parallel_tbl %>%
    modeltime_residuals_test(dataset) %>%
    filter(.model_id == model_best_id)
}

# 8. Other specifications -------------------------------------------------

get_model_configuration <- function(model_grid, model_best_id){
  model_configuration <- model_grid[model_best_id,]
}


# 9. Predict New Data -----------------------------------------------------

get_new_predictions <- function(date_var, model_parallel_tbl, new_data, model_best_id, KPI){
  new_predictions <- tibble("Date" = date_var,
         "Predicted" = model_parallel_tbl %>%
                           modeltime_forecast(new_data = new_data) %>%
                           filter(.model_id == model_best_id) %>%
                           select(.value) %>% pull(),
         "Real" = KPI %>% pull()) %>%
    mutate(Residuals = Real - Predicted)
}





