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


# 1. Set the model  -------------------------------------------------------

# Model Specification ====

set_xg_model_grid <- function(n){
  # Selects the number of models created
  xg_model_grid <- grid_random(parameters(boost_tree(trees = tune(),# The cv will select the number of trees
                                                     min_n = tune(),# The cv will select the min samples
                                                     tree_depth = tune(),# tree maximum depth
                                                     sample_size = tune())), # tree maximum depth
                               size = n)
  return(xg_model_grid)
}

xg_model_table_set <- function(model_grid){
  # This function uses the model grid created and specifies the model

  xg_model_tbl <- model_grid %>%
    create_model_grid(
      f_model_spec = boost_tree,
      engine_name  = "xgboost",
      mode         = "regression"
    )
  return(xg_model_tbl)
}

# Get the feature importances ------------------------------------------

get_xg_importances <- function(model_base){
  model_predictors <- model_base %>%
    xgboost::xgb.importance(model = .,
                            feature_names = NULL)
  return(model_predictors)
}

plot_xg_importances <- function(model_predictors){
  plotly::ggplotly(ggplot(model_predictors) +
                     aes(x = reorder(Feature,Gain), fill = Feature, weight = Gain) +
                     geom_bar() +
                     scale_colour_gradient(low = "#132B43",
                                           high = "#56B1F7") +
                     labs(x = "Variable", y = "Importance (Percentage of appearance)", title = "Variable Importance") +
                     coord_flip() +
                     hrbrthemes::theme_ipsum_ps() +
                     theme(legend.position = "none", plot.title = element_text(face = "bold",
                                                                               hjust = 0.5),
                           axis.title.y = element_text(size = 13L, face = "bold"),
                           axis.title.x = element_text(size = 13L, face = "bold", hjust = 1)))
}























