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

#source("R/both_model_functions.R")
#source("R/xg_model_functions.R")
#source("R/rf_model_functions.R")


server <- function(input,output,session){
  modeltime::parallel_start(8, .method = "parallel")

  url <- a('The GitHub Repository', href = "https://github.com/jdcarranzas/RunForestRun")

  output$github <- renderUI({tagList("Want to check what's going on in the backseat? Visit:", url)})

  # Upload Tab =====

  data_model <- reactive({

    req(input$file) #Reads the file

    inFile <- input$file # Takes the file

    data_review_mod <- read.xlsx(inFile$datapath, detectDates = T) # Assigned to a variable

    data_review_1 <- data_review_mod %>%
      select_if(is.numeric) %>% # Select only if its numeric
      select_if(function(col) sum(abs(col), na.rm = T) != 0) %>% # Removes 0 variables
      select_if(function(col) var(col, na.rm = T) != 0) # Removes zero variance variables

    updateSelectInput(session,'cat','Variable that defines your Segments',names(select_if(data_review_mod, is.character))) # Select Variable with products

    updateSelectInput(session,'KPI','Select your KPI',names(select_if(data_review_mod, is.numeric))) #Select the kpi from the list

    updateSelectInput(session, inputId = 'drop', label = 'Drop variables from the model:',
                      choices = names(select_if(data_review_mod, is.numeric)))

    updateSelectInput(session,'KPI_xg','Select your XG KPI',names(select_if(data_review_mod, is.numeric))) #Select the kpi from the list

    updateSelectInput(session, inputId = 'drop_xg', label = 'Drop variables from the XG model:',
                      choices = names(select_if(data_review_mod, is.numeric)))

    return(data_review_mod)

  })

  output$variableUI <- renderUI({
    selectInput(inputId = "variable", label = "Segment", choices = data_model()[,input$cat]) #Selects the variable with the segments
  })

  data_int <- reactive({
    data_model() %>%
      filter(get(input$cat)==input$variable) # Filters to the segment of interest
  })

  output$glimpse <- renderDataTable({
    glimpse(data_int())  # Renders the datatable
  })

  output$summary <- renderDataTable({
    skim(data_int() %>%
           select_if(is.numeric))  # Renders the datatable
  })

  data_new <- reactive({

    req(input$file_new) #Reads the file

    inFile <- input$file_new # Takes the file

    data_actual <- read.xlsx(inFile$datapath, detectDates = T) %>%
      filter(get(input$cat) == input$variable)# Assigned to a variable

    return(data_actual)

  })

  output$glimpse_new <- renderDataTable(
    glimpse(data_new())
  )

  missing_vars <- reactive({
    pivot_older <- data_int() %>%
      select_if(function(col) inherits(col, 'Date') | is.numeric(col)) %>%
      pivot_longer(cols = where(is.numeric), names_to = 'Variable', values_to = 'Values')

    pivot_newer <- data_new() %>%
      select_if(function(col) inherits(col, 'Date') | is.numeric(col)) %>%
      pivot_longer(cols = where(is.numeric), names_to = 'Variable', values_to = 'Values')

    missing_older <- pivot_older %>%
      anti_join(pivot_newer, by = 'Variable') %>%
      select(Variable)

    missing_newer <- pivot_newer %>%
      anti_join(pivot_older, by = 'Variable') %>%
      select(Variable)

    missing_vars <- missing_older %>%
      bind_rows(missing_newer) %>%
      unique()

    rm(pivot_older, pivot_newer, missing_older, missing_newer)

    return(missing_vars)
  })

  output$missing <- renderDataTable({missing_vars()})

  data_int_fin <- reactive({
    data_int() %>%
      select(-one_of(c(missing_vars() %>% pull())))
  })

  # Random Forest tab ====

  reactive({set.seed(input$seed)})

  # Create model table
  rf_model_tbl <- reactive({
    rf_model_table_set(set_rf_model_grid(input$n_models_rf))
  })

  rf_drop_variables <- eventReactive(input$update_drop_rf, {input$drop})

  # Create dataset split
  split_dataset <- reactive({
    split(data_int_fin() %>%
            select(-c(rf_drop_variables())),
          input$train_threshold)
  })

  # Create model recipe
  rf_model_recipe <- reactive({
     set_recipe(formula = as.formula(paste(input$KPI, "~.")),
                split_object = split_dataset(),
                threshold = input$thresh_corr)
  })

  # Create Workflow set
  rf_model_wfset <- reactive({
    set_model_wfset(rf_model_recipe(),
                    rf_model_tbl())
  })


  # Train the model
  rf_model_parallel_tbl <- reactive({
    set_model_parallel_tbl(dataset = data_int_fin() %>%
                             select(-c(rf_drop_variables())),
                           model_wfset = rf_model_wfset(),
                           split_object = split_dataset())
  })

  # Get the ID for the best model
  rf_model_best <- reactive({
    get_best_model(rf_model_parallel_tbl())
  })

  # Get the base model
  rf_model_base <- reactive({
    get_base_model(rf_model_best(),
                   rf_model_parallel_tbl())
  })

  # Get the feature importances plot
  output$rf_importances <- renderPlotly({
    plot_rf_importances(get_rf_importances(rf_model_base()))
  })

  # Get the predictions dataset
  rf_model_predictions <- reactive({
    get_model_predictions(date_var = select_if(data_int_fin(), is.Date),
                          model_parallel_tbl = rf_model_parallel_tbl(),
                          model_best_id = rf_model_best(),
                          split_object = split_dataset(),
                          KPI = select(data_int_fin(), input$KPI))
  })

  # Get the predictions plot
  output$rf_predictions <- renderPlotly({
    plot_model_predictions(rf_model_predictions(), data_int_fin() %>% select_if(is.Date) %>% slice_tail(n=1) %>% pull())  # Renders the datatable
  })

  # Get the model metrics
  rf_model_metrics <- reactive({
    get_model_metrics(rf_model_parallel_tbl(), rf_model_best(), newdata = NULL)
  })

  # Render Model Metrics
  output$rf_m_metrics <- renderDataTable(
    rf_model_metrics() %>% select(-c(.model_id, .model_desc, .type)) %>% mutate(across(everything(), function(x) round(x,2)))
  )

  # Get the errors metrics
  rf_error_metrics <- reactive({
    get_error_metrics(rf_model_parallel_tbl(),
                      data_int_fin(),
                      rf_model_best())
  })

  # Render error metrics
  output$rf_e_metrics <- renderDataTable(
    rf_error_metrics() %>% select(-c(.model_id, .model_desc)) %>% mutate(across(everything(), function(x) round(x,2)))
  )

  # Get the configuration
  output$rf_configuration <- renderDataTable(
    get_model_configuration(set_rf_model_grid(input$n_models_rf), rf_model_best())
  )

  # Predict for new data
  rf_model_predictions_new <- reactive({
    get_new_predictions(date_var = data_new() %>% select_if(is.Date),
                        model_parallel_tbl = rf_model_parallel_tbl(),
                        new_data = data_new(),
                        model_best_id = rf_model_best(),
                        KPI = data_new() %>% select(input$KPI))
  })

  # Test

  # Plot for new data
  output$rf_predictions_new <- renderPlotly({
    plot_model_predictions(rf_model_predictions_new(), data_int_fin() %>% select_if(is.Date) %>% slice_tail(n=1) %>% pull())  # Renders the datatable
  })

  # Metrics for new data
  rf_new_e_metrics_tbl <- reactive({
    get_error_metrics(rf_model_parallel_tbl(),
                      data_new(),
                      rf_model_best()) %>%
      select(-c(.model_id, .model_desc)) %>%
      mutate(across(everything(), function(x) round(x,2)))
  })


  output$rf_new_e_metrics <- renderDataTable(
    rf_new_e_metrics_tbl()
  )

  rf_new_m_metrics_tbl <- reactive({
    get_model_metrics(rf_model_parallel_tbl(),
                      rf_model_best(),
                      newdata = data_new()) %>%
      select(-c(.model_id, .model_desc, .type)) %>%
      mutate(across(everything(), function(x) round(x,2)))
  })

  output$rf_new_m_metrics <- renderDataTable(
    rf_new_m_metrics_tbl()
  )

  # Download model results
  output$download_rf_results <- downloadHandler(
    filename = function(){
      'model_rf_results.xlsx'
    },
    content = function(file){
      list_data <- list('Model_Results' = rf_model_predictions_new() %>% tibble(),
                        'Variable_Importances' = get_rf_importances(rf_model_base()) %>% tibble(),
                        'Model_Metrics' = rf_new_m_metrics_tbl() %>% tibble(),
                        'Error_Metrics' = rf_new_e_metrics_tbl() %>% tibble(),
                        'Model_Config' = get_model_configuration(set_rf_model_grid(input$n_models_rf), rf_model_best()) %>% tibble()
      )
      write_xlsx(list_data, file)
    })




  # Extreme Gradient Boost tab ====

  reactive({set.seed(input$seed_xg)})

  # Create model table
  xg_model_tbl <- reactive({
    xg_model_table_set(set_xg_model_grid(input$n_models_xg))
  })

  xg_drop_variables <- eventReactive(input$update_drop_xg, {input$drop_xg})

  # Create dataset split
  xg_split_dataset <- reactive({
    split(data_int_fin()%>%
            select(-c(xg_drop_variables())),
          input$train_threshold_xg)
  })

  # Create model recipe
  xg_model_recipe <- reactive({
    set_recipe(formula = as.formula(paste(input$KPI_xg, "~.")),
               split_object = xg_split_dataset(),
               threshold = input$thresh_corr_xg)
  })

  # Create Workflow set
  xg_model_wfset <- reactive({
    set_model_wfset(xg_model_recipe(),
                    xg_model_tbl())
  })

  # Train the model
  xg_model_parallel_tbl <- reactive({
    set_model_parallel_tbl(dataset = data_int_fin()%>%
                             select(-c(xg_drop_variables())),
                           model_wfset = xg_model_wfset(),
                           split_object = xg_split_dataset())
  })

  # Get the ID for the best model
  xg_model_best <- reactive({
    get_best_model(xg_model_parallel_tbl())
  })

  # Get the base model
  xg_model_base <- reactive({
    get_base_model(xg_model_best(),
                   xg_model_parallel_tbl())
  })

  # Get the feature importances plot
  output$xg_importances <- renderPlotly({
    plot_xg_importances(get_xg_importances(xg_model_base()))
  })

  # Get the predictions dataset
  xg_model_predictions <- reactive({
    get_model_predictions(date_var = select_if(data_int_fin(), is.Date),
                          model_parallel_tbl = xg_model_parallel_tbl(),
                          model_best_id = xg_model_best(),
                          split_object = xg_split_dataset(),
                          KPI = select(data_int_fin(), input$KPI_xg))
  })

  # Get the predictions plot
  output$xg_predictions <- renderPlotly({
    plot_model_predictions(xg_model_predictions(), data_int_fin() %>% select_if(is.Date) %>% slice_tail(n=1) %>% pull())  # Renders the datatable
  })

  # Get the model metrics
  xg_model_metrics <- reactive({
    get_model_metrics(xg_model_parallel_tbl(), xg_model_best(), newdata = NULL) %>%
      select(-c(.model_id, .model_desc, .type)) %>%
      mutate(across(everything(), function(x) round(x,2)))
  })

  # Render Model Metrics
  output$xg_m_metrics <- renderDataTable(
    xg_model_metrics()
  )

  # Get the errors metrics
  xg_error_metrics <- reactive({
    get_error_metrics(xg_model_parallel_tbl(),
                      data_int_fin(),
                      xg_model_best()) %>%
      select(-c(.model_id, .model_desc)) %>%
      mutate(across(everything(), function(x) round(x,2)))
  })

  # Render error metrics
  output$xg_e_metrics <- renderDataTable(
    xg_error_metrics()
  )

  # Get the configuration
  output$xg_configuration <- renderDataTable(
    get_model_configuration(set_xg_model_grid(input$n_models_xg), xg_model_best())
  )

  xg_model_predictions_new <- reactive({
    get_new_predictions(date_var = data_new() %>% select_if(is.Date),
                        model_parallel_tbl = xg_model_parallel_tbl(),
                        new_data = data_new(),
                        model_best_id = xg_model_best(),
                        KPI = data_new() %>% select(input$KPI_xg))
  })

  # Test

  # Plot for new data
  output$xg_predictions_new <- renderPlotly({
    plot_model_predictions(xg_model_predictions_new(), data_int_fin() %>% select_if(is.Date) %>% slice_tail(n=1) %>% pull())  # Renders the datatable
  })

  # Metrics for new data

  xg_new_e_metrics_tbl <- reactive({
    get_error_metrics(xg_model_parallel_tbl(),
                      data_new(),
                      xg_model_best()) %>%
      select(-c(.model_id, .model_desc)) %>%
      mutate(across(everything(), function(x) round(x,2)))
  })

  output$xg_new_e_metrics <- renderDataTable({
    xg_new_e_metrics_tbl()
  })

  xg_new_m_metrics_tbl <- reactive({
    get_model_metrics(xg_model_parallel_tbl(),
                      xg_model_best(),
                      newdata = data_new()) %>%
      select(-c(.model_id, .model_desc, .type)) %>%
      mutate(across(everything(), function(x) round(x,2)))
  })


  output$xg_new_m_metrics <- renderDataTable(
    xg_new_m_metrics_tbl()
  )

  # Download model results
  output$download_xg_results <- downloadHandler(
    filename = function(){
      'model_xg_results.xlsx'
    },
    content = function(file){
      list_data <- list('Model_Results' = xg_model_predictions_new() %>% tibble(),
                        'Variable_Importances' = get_xg_importances(xg_model_base()) %>% tibble(),
                        'Model_Metrics' = xg_new_m_metrics_tbl() %>% tibble(),
                        'Error_Metrics' = xg_new_e_metrics_tbl() %>% tibble(),
                        'Model_Config' = get_model_configuration(set_xg_model_grid(input$n_models_xg), xg_model_best()) %>% tibble()
      )
      write_xlsx(list_data, file)
    })
}
