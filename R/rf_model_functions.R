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
require(shiny)
require(randomForest)


# Model Specification ====

set_rf_model_grid <- function(n){
  # Selects the number of models created
  rf_model_grid <- grid_random(parameters(rand_forest(trees = tune(), # The cv will select the number of trees
                                                   min_n = tune())), # The cv will select the min samples
                            size = n)
  return(rf_model_grid)
}

rf_model_table_set <- function(model_grid){
  # This function uses the model grid created and specifies the model

  rf_model_tbl <- model_grid %>%
    create_model_grid(
      f_model_spec = rand_forest,
      engine_name  = "randomForest",
      mode         = "regression"
    )
  return(rf_model_tbl)
}

# Get the feature importances ------------------------------------------

get_rf_importances <- function(model_base){
  model_predictors <- model_base %>%
    randomForest::importance() %>%
    tibble(Name = rownames(.),
           Variable_Importance = as.numeric(.)) %>%
    select(2:3) %>%
    mutate(Variable_Importance = scales::rescale(Variable_Importance, to = c(0,1))) %>%
    arrange(desc(Variable_Importance))
  return(model_predictors)
}

plot_rf_importances <- function(model_predictors){
  plotly::ggplotly(ggplot(model_predictors) +
                             aes(x = reorder(Name,Variable_Importance), fill = Name, weight = Variable_Importance) +
                             geom_bar() +
                             scale_colour_gradient(low = "#132B43",
                                                   high = "#56B1F7") +
                             labs(x = "Variable", y = "Importance (scaled from 0 to 1)", title = "Variable Importance") +
                             coord_flip() +
                             hrbrthemes::theme_ipsum_ps() +
                             theme(legend.position = "none", plot.title = element_text(face = "bold",
                                                                                       hjust = 0.5),
                                   axis.title.y = element_text(size = 13L, face = "bold"),
                                   axis.title.x = element_text(size = 13L, face = "bold", hjust = 1)))

}











