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

size = 2


# About App ---------------------------------------------------------------
Tab1 <- tabPanel("About the app", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(h3('Introduction'), width = size),
                               mainPanel(
                                 br(),
                                 p('escribir algo para la introduccion, pero que eso lo haga el pasante xdxd')
                              )))


# Upload App --------------------------------------------------------------
Tab2 <- tabPanel("Upload Dataset", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(
                                 br(),
                                 h4("1. Choose your excel file"),
                                 fileInput('file', 'Choose Excel File', accept = c('.xlsx')),
                                 hr(),
                                 h4('2. Define your segment'),
                                 p('To continue, please select the variable name that defines your segments.'),
                                 selectInput('cat','Segment Variable', '', selected=''),
                                 p('To continue, please select a segment.'),
                                 htmlOutput("variableUI"),
                                 hr(),
                                 h4("3. Select a new database for contrasting (optional)"),
                                 fileInput('file_new', 'Choose New File', accept = c('.xlsx')),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h3('Reviewing your Dataset'),
                                 br(),
                                 dataTableOutput('glimpse'),
                                 br(),
                                 h3('A brief summary'),
                                 dataTableOutput('summary'),
                                 h3('Reviewing the new Dataset'),
                                 br(),
                                 dataTableOutput('glimpse_new')
                               )))


# Random Forest App -------------------------------------------------------
Tab3 <- tabPanel("Random Forest", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(
                                 br(),
                                 sliderInput(inputId = "seed",
                                             label = "Model and Grid Seed:",
                                             min = 200,
                                             max = 300,
                                             val = 250),
                                 sliderInput(inputId = "train_threshold",
                                             label = "Train Proportion:",
                                             min = 0.7,
                                             max = 0.9,
                                             val = 0.8),
                                 selectInput(inputId = "KPI",
                                             label   = "Select your KPI",
                                             "",
                                             selected = ""),
                                 selectInput(inputId = "drop",
                                             label = "Drop variables from the model:",
                                             "",
                                             multiple = T),
                                 br(),
                                 actionButton("update_drop_rf", "Drop Variables"),
                                 p('When you have selected all the variables you want to drop,
                                   press the button in order to re-run the model.'),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h4('Some Pargraph'),
                                 p('some text, idk'),
                                 splitLayout(
                                   sliderInput(inputId = "n_models_rf",
                                               label = "Select number of models to test",
                                               min = 5,
                                               max = 30,
                                               val = 8),
                                   sliderInput(inputId = "thresh_corr",
                                               label = "Select correlation threshold",
                                               min = 0.4,
                                               max = 1,
                                               val = 0.7)),
                                 br(),
                                 h4("Model Results"),
                                 splitLayout(
                                   plotlyOutput("rf_importances"),
                                   plotlyOutput("rf_predictions")
                                 ),
                                 h4("Model Metrics"),
                                 hr(),
                                 dataTableOutput("rf_m_metrics"),
                                 h4("Error-Based Metrics"),
                                 dataTableOutput("rf_e_metrics"),
                                 h4("Best Model Configuration"),
                                 dataTableOutput("rf_configuration"),
                                 fluidRow(
                                   column(
                                     width = 5,
                                     dataTableOutput("rf_new_m_metrics"),
                                     dataTableOutput("rf_new_e_metrics")
                                   ),
                                   column(
                                     width = 7,
                                     plotlyOutput("rf_predictions_new")
                                   )
                                 ),
                                 downloadButton("download_rf_results", "Download Results")
                               )))

Tab4 <- tabPanel("XGBoost", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(
                                 br(),
                                 sliderInput(inputId = "seed_xg",
                                             label = "Model and Grid Seed:",
                                             min = 200,
                                             max = 300,
                                             val = 250),
                                 sliderInput(inputId = "train_threshold_xg",
                                             label = "Train Proportion:",
                                             min = 0.7,
                                             max = 0.9,
                                             val = 0.8),
                                 selectInput(inputId = "KPI_xg",
                                             label   = "Select your XG KPI",
                                             "",
                                             selected = ""),
                                 selectInput(inputId = "drop_xg",
                                             label = "Drop variables from the XG model:",
                                             "",
                                             multiple = T),
                                 br(),
                                 actionButton("update_drop_xg", "Drop XG Variables"),
                                 p('When you have selected all the variables you want to drop,
                                   press the button in order to re-run the model.'),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h4('Some Pargraph'),
                                 p('some text, idk'),
                                 splitLayout(
                                   sliderInput(inputId = "n_models_xg",
                                               label = "Select number of models to test",
                                               min = 5,
                                               max = 30,
                                               val = 8),
                                   sliderInput(inputId = "thresh_corr_xg",
                                               label = "Select correlation threshold",
                                               min = 0.4,
                                               max = 1,
                                               val = 0.7)),
                                 br(),
                                 h4("Model Results"),
                                 splitLayout(
                                   plotlyOutput("xg_importances"),
                                   plotlyOutput("xg_predictions")
                                 ),
                                 h4("Model Metrics"),
                                 hr(),
                                 dataTableOutput("xg_m_metrics"),
                                 h4("Error-Based Metrics"),
                                 dataTableOutput("xg_e_metrics"),
                                 h4("Best Model Configuration"),
                                 dataTableOutput("xg_configuration"),
                                 fluidRow(
                                   column(
                                     width = 5,
                                     dataTableOutput("xg_new_m_metrics"),
                                     dataTableOutput("xg_new_e_metrics")
                                   ),
                                   column(
                                     width = 7,
                                     plotlyOutput("xg_predictions_new")
                                   )
                                 ),
                                 downloadButton("download_xg_results", "Download Results")
                               )))

Tab5 <- tabPanel("HoliUWU")









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

size = 2


# About App ---------------------------------------------------------------
Tab1 <- tabPanel("About the app", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(h3('Introduction'), width = size),
                               mainPanel(
                                 br(),
                                 p('escribir algo para la introduccion, pero que eso lo haga el pasante xdxd')
                              )))


# Upload App --------------------------------------------------------------
Tab2 <- tabPanel("Upload Dataset", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(
                                 br(),
                                 h4("1. Choose your excel file"),
                                 fileInput('file', 'Choose Excel File', accept = c('.xlsx')),
                                 hr(),
                                 h4('2. Define your segment'),
                                 p('To continue, please select the variable name that defines your segments.'),
                                 selectInput('cat','Segment Variable', '', selected=''),
                                 p('To continue, please select a segment.'),
                                 htmlOutput("variableUI"),
                                 hr(),
                                 h4("3. Select a new database for contrasting (optional)"),
                                 fileInput('file_new', 'Choose New File', accept = c('.xlsx')),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h3('Reviewing your Dataset'),
                                 br(),
                                 dataTableOutput('glimpse'),
                                 br(),
                                 h3('A brief summary'),
                                 dataTableOutput('summary'),
                                 h3('Reviewing the new Dataset'),
                                 br(),
                                 dataTableOutput('glimpse_new')
                               )))


# Random Forest App -------------------------------------------------------
Tab3 <- tabPanel("Random Forest", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(
                                 br(),
                                 sliderInput(inputId = "seed",
                                             label = "Model and Grid Seed:",
                                             min = 200,
                                             max = 300,
                                             val = 250),
                                 sliderInput(inputId = "train_threshold",
                                             label = "Train Proportion:",
                                             min = 0.7,
                                             max = 0.9,
                                             val = 0.8),
                                 selectInput(inputId = "KPI",
                                             label   = "Select your KPI",
                                             "",
                                             selected = ""),
                                 selectInput(inputId = "drop",
                                             label = "Drop variables from the model:",
                                             "",
                                             multiple = T),
                                 br(),
                                 actionButton("update_drop_rf", "Drop Variables"),
                                 p('When you have selected all the variables you want to drop,
                                   press the button in order to re-run the model.'),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h4('Some Pargraph'),
                                 p('some text, idk'),
                                 splitLayout(
                                   sliderInput(inputId = "n_models_rf",
                                               label = "Select number of models to test",
                                               min = 5,
                                               max = 30,
                                               val = 8),
                                   sliderInput(inputId = "thresh_corr",
                                               label = "Select correlation threshold",
                                               min = 0.4,
                                               max = 1,
                                               val = 0.7)),
                                 br(),
                                 h4("Model Results"),
                                 splitLayout(
                                   plotlyOutput("rf_importances"),
                                   plotlyOutput("rf_predictions")
                                 ),
                                 h4("Model Metrics"),
                                 hr(),
                                 dataTableOutput("rf_m_metrics"),
                                 h4("Error-Based Metrics"),
                                 dataTableOutput("rf_e_metrics"),
                                 h4("Best Model Configuration"),
                                 dataTableOutput("rf_configuration"),
                                 fluidRow(
                                   column(
                                     width = 5,
                                     dataTableOutput("rf_new_m_metrics"),
                                     dataTableOutput("rf_new_e_metrics")
                                   ),
                                   column(
                                     width = 7,
                                     plotlyOutput("rf_predictions_new")
                                   )
                                 ),
                                 downloadButton("download_rf_results", "Download Results")
                               )))

Tab4 <- tabPanel("XGBoost", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(
                                 br(),
                                 sliderInput(inputId = "seed_xg",
                                             label = "Model and Grid Seed:",
                                             min = 200,
                                             max = 300,
                                             val = 250),
                                 sliderInput(inputId = "train_threshold_xg",
                                             label = "Train Proportion:",
                                             min = 0.7,
                                             max = 0.9,
                                             val = 0.8),
                                 selectInput(inputId = "KPI_xg",
                                             label   = "Select your XG KPI",
                                             "",
                                             selected = ""),
                                 selectInput(inputId = "drop_xg",
                                             label = "Drop variables from the XG model:",
                                             "",
                                             multiple = T),
                                 br(),
                                 actionButton("update_drop_xg", "Drop XG Variables"),
                                 p('When you have selected all the variables you want to drop,
                                   press the button in order to re-run the model.'),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h4('Some Pargraph'),
                                 p('some text, idk'),
                                 splitLayout(
                                   sliderInput(inputId = "n_models_xg",
                                               label = "Select number of models to test",
                                               min = 5,
                                               max = 30,
                                               val = 8),
                                   sliderInput(inputId = "thresh_corr_xg",
                                               label = "Select correlation threshold",
                                               min = 0.4,
                                               max = 1,
                                               val = 0.7)),
                                 br(),
                                 h4("Model Results"),
                                 splitLayout(
                                   plotlyOutput("xg_importances"),
                                   plotlyOutput("xg_predictions")
                                 ),
                                 h4("Model Metrics"),
                                 hr(),
                                 dataTableOutput("xg_m_metrics"),
                                 h4("Error-Based Metrics"),
                                 dataTableOutput("xg_e_metrics"),
                                 h4("Best Model Configuration"),
                                 dataTableOutput("xg_configuration"),
                                 fluidRow(
                                   column(
                                     width = 5,
                                     dataTableOutput("xg_new_m_metrics"),
                                     dataTableOutput("xg_new_e_metrics")
                                   ),
                                   column(
                                     width = 7,
                                     plotlyOutput("xg_predictions_new")
                                   )
                                 ),
                                 downloadButton("download_xg_results", "Download Results")
                               )))

Tab5 <- tabPanel("HoliUWU")









