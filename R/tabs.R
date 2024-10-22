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
require(shiny)
require(dendextend)

size = 2

# About App tab ---------------------------------------------------------------
Tab1 <- tabPanel("About the app", fluid = T,
                 sidebarLayout(position = 'left',
                               sidebarPanel(h3('Introduction'), width = size),
                               mainPanel(
                                 h3('Welcome to the jungle'),
                                 br(),
                                 p('This application was developed in order to help you and your team the
                                   process of selecting adequate predictors for your MMM model. This process
                                   is thought to be an insight-recomendation generator for your linear model
                                   and improve the understanding of your independent predictors.', style = "font-size:18px"),
                                 br(),
                                 p(strong('Run Forest Run'), 'is an useful graphic interface for creating
                                   modelling rules, to facilitate the understanding of your newest model, but
                                   it actually excels at comparing the updates vs the old model. It gives a
                                   sense of entropy between the variables measured before the previous threshold
                                   and the new data points.', style = "font-size:18px"),
                                 br(),
                                 p('To continue using the app please make sure that both of your datasets:', style = "font-size:18px"),
                                 HTML(
                                   '<ul>
                                   <li style = "font-size:18px"; >Are Excel files (.xlsx).</li>
                                   <li style = "font-size:18px"; >Does not contain any missing values.</li>
                                   <li style = "font-size:18px"; >Its first row contains the variable names.</li>
                                   <li style = "font-size:18px"; >Each of its columns is populated from the second row on.</li>
                                   <li style = "font-size:18px"; >Contains a Date column that is in any date format.</li>
                                   <li style = "font-size:18px"; >Both of your datasets have the exact same names.</li>
                                   </ul>'),
                                 h3('Example:'),
                                 br(),
                                 img(src='R\a.PNG',height=600,width=900),
                                 br(),
                                 br(),
                                 br(),
                                 p('If you have questions please contact David Carranza (Juan.Carranza@kinesso.com)', style="font-size:18px"),
                                 p(uiOutput('github'), style="font-size:18px")
                              )))


# Upload tab --------------------------------------------------------------
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
                                 br(),
                                 dataTableOutput('summary'),
                                 br(),
                                 h3('Reviewing the new Dataset'),
                                 br(),
                                 dataTableOutput('glimpse_new'),
                                 br(),
                                 h3('This variables will not appear in your model'),
                                 p("This happens because these variables are not present in one
                                   of the datasets, it could be that in the new data we don't have
                                   all the variables used in the last model, or because there are new
                                   variables in this update. For clarification, please visit the",
                                   strong('Data Review App'), style="font-size:18px"),
                                 br(),
                                 dataTableOutput('missing')
                               )))


# XGBoost tab -------------------------------------------------------
Tab3 <- tabPanel("XGBoost", fluid = T,
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
                                 actionButton("update_drop_xg", "Run the XG Model"),
                                 p('Once you have checked the modifiable parameters, press the button to
                                   start the model'),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h3('Extreme Gradient Boost Model'),
                                 p('This non-linear model is a set of decision trees. In particular, this model
                                   starts with a base predictor (or base tree) and adds trees sequentially, i.e.
                                   it adds one tree after another in order to adjust the errors of the predictor
                                   model. The models are fitted by using any arbitrary differentiable loss function and
                                   using the gradient boosting algorithm (hence the name', strong('gradient boosting'),').
                                   Very similar to the optimisation process performed in neural networks.',
                                   style="font-size:18px"),
                                 br(),
                                 p('The use of this application consists of evaluating the number of models defined
                                   by the user (we strongly recommend a maximum of', strong('6 models'),'), giving as input the
                                   dependent variable of interest and this will be adjusted against the other predictors.
                                   The predictors may sometimes have correlations with each other, so the',
                                   strong('correlation threshold slider'), 'defines the maximum tolerable linear
                                   relationship between the predictors in the model. It is also possible to remove
                                   variables that may generate noise, or that the user simply does not want. Once
                                   these parameters have been configured, press the', strong("Run the model"),
                                   'button to run the workflow.', style="font-size:18px"),
                                 splitLayout(
                                   sliderInput(inputId = "n_models_xg",
                                               label = "Select number of models to test",
                                               min = 2,
                                               max = 8,
                                               val = 4),
                                   sliderInput(inputId = "thresh_corr_xg",
                                               label = "Select correlation threshold",
                                               min = 0.4,
                                               max = 1,
                                               val = 0.7)),
                                 br(),
                                 h3("Model Results"),
                                 p('The', strong('Actual vs Predicted'), 'plot shows you the model result in contrast
                                   with the real KPI variable, while the', strong('Variable Importance'), 'plot shows
                                   the percentage of appearance of any variable. In other terms, in how many trees the
                                   variables appear. So, if a variable has a huge impact in your model, it may be on top
                                   of the chart.', style="font-size:18px"),
                                 p(strong('Warning:'), 'In some cases, the', strong('Variable Importance'), 'plot may
                                   not display, for solving this issue, slide the number of models once. If this error
                                   persists, feel free to call me. This happens because the model does not converge, so
                                   updating the parameters will solve this issue.', style="font-size:18px"),
                                 plotlyOutput("xg_importances"),
                                 plotlyOutput("xg_predictions"),
                                 br(),
                                 p('The model metrics and the error metrics are presented in the next two datatables.
                                   The values are the final statistics. We usally use the', strong('MAPE'), 'the ',
                                   strong("R-Squared"), '(presented in the model metrics table as "rsq") and most
                                   recently the', strong('Durbin-Watson.'), style="font-size:18px"),
                                 h3("Model Metrics"),
                                 br(),
                                 dataTableOutput("xg_m_metrics"),
                                 br(),
                                 h3("Error-Based Metrics"),
                                 br(),
                                 dataTableOutput("xg_e_metrics"),
                                 br(),
                                 h3("Best Model Configuration"),
                                 p('This is the best performing-model. The parameters are explained below:',
                                   style="font-size:18px"),
                                 HTML(
                                   '<ul>
                                   <li style = "font-size:18px"; >Trees refers to the number of trees used for fitting the model.</li>
                                   <li style = "font-size:18px"; >Min_n is the minimum number of observations taken for each model.</li>
                                   <li style = "font-size:18px"; >Tree Depth is the max number of descending nodes for each model.</li>
                                   <li style = "font-size:18px"; >Sample Size is the percentage of observations exposed for each model.</li>
                                   <li style = "font-size:18px"; >Note: The number of features
                                   taken for each model is equal to the square root of total features.</li>
                                   </ul>'),
                                 br(),
                                 dataTableOutput("xg_configuration"),
                                 br(),
                                 h3('Out-Of-Sample predictions'),
                                 br(),
                                 p("In this section you can see the results of training the model with original data
                                   and predicting the outcome with new data that the model hasn't seen. This is quite useful
                                   because when we update the models, we could have a lot of problems with the new data points
                                   and may waste a lot more of time. This can give you a better perspective about the new
                                   information given by the client. If the model", strong("crashes"), "(the previous statistics
                                   deteriorates a lot) the new data points could suggest a change in structure of the model,
                                   suggesting that it may be necessary remodelling; if not, it could just be a routine update.
                                   The blue vertical line shows the moment where the new data is taken",style="font-size:18px"),
                                 br(),
                                 plotlyOutput("xg_predictions_new"),
                                 br(),
                                 fluidRow(
                                   column(
                                     h3('Model Metrics'),
                                     br(),
                                     dataTableOutput("xg_new_m_metrics"),
                                     br(),
                                     width = 6,
                                   ),
                                   column(
                                     h3('Error Metrics'),
                                     br(),
                                     dataTableOutput("xg_new_e_metrics"),
                                     br(),
                                     width = 6
                                   )
                                 ),
                                 br(),
                                 h3('Download your results'),
                                 p('Are you satisfied with the model? Download it!', style="font-size:18px"),
                                 downloadButton("download_xg_results", "Download Results")
                               )))

# Random Forest tab -------------------------------------------------------
Tab4 <- tabPanel("Random Forest", fluid = T,
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
                                 actionButton("update_drop_rf", "Run the RF Model"),
                                 p('Once you have checked the modifiable parameters, press the button to
                                   start the model'),
                                 width = size),
                               mainPanel(
                                 br(),
                                 h3('Random Forest Model'),
                                 p("This non-linear model is a set of decision trees. In contrast to the previous model, this
                                   model does not use the decision trees sequentially, but each of the trees operates independently.
                                   Each gives an individual outcome and the model's outcome is then the top-ranked outcome
                                   across all regression trees.  By being independent, each model protects the other from
                                   individual errors, improving aggregate performance.", style="font-size:18px"),
                                 br(),
                                 p('The use of this application consists of evaluating the number of models defined
                                   by the user (we strongly recommend a maximum of', strong('8 models'),'), giving as input the
                                   dependent variable of interest and this will be adjusted against the other predictors.
                                   The predictors may sometimes have correlations with each other, so the',
                                   strong('correlation threshold slider'), 'defines the maximum tolerable linear
                                   relationship between the predictors in the model. It is also possible to remove
                                   variables that may generate noise, or that the user simply does not want. Once
                                   these parameters have been configured, press the', strong("Run the model"),
                                   'button to run the workflow.', style="font-size:18px"),
                                 br(),
                                 splitLayout(
                                   sliderInput(inputId = "n_models_rf",
                                               label = "Select number of models to test",
                                               min = 2,
                                               max = 8,
                                               val = 4),
                                   sliderInput(inputId = "thresh_corr",
                                               label = "Select correlation threshold",
                                               min = 0.4,
                                               max = 1,
                                               val = 0.7)),
                                 br(),
                                 h3("Model Results"),
                                 p('The', strong('Actual vs Predicted'), 'plot shows you the model result in contrast
                                   with the real KPI variable, while the', strong('Variable Importance'), 'plot shows
                                   the total number of appearance of any variable. In other terms, in how many trees the
                                   variables appear, normalized into (0,1). So, if a variable has a huge impact in your model,
                                   it may be on top of the chart. Like the previous model.', style="font-size:18px"),
                                 br(),
                                 plotlyOutput("rf_importances"),
                                 plotlyOutput("rf_predictions"),
                                 br(),
                                 p('The model metrics and the error metrics are presented in the next two datatables.
                                   The values are the final statistics. We usally use the', strong('MAPE'), 'the ',
                                   strong("R-Squared"), '(presented in the model metrics table as "rsq") and most
                                   recently the', strong('Durbin-Watson.'), style="font-size:18px"),
                                 br(),
                                 h3("Model Metrics"),
                                 br(),
                                 dataTableOutput("rf_m_metrics"),
                                 br(),
                                 h3("Error-Based Metrics"),
                                 br(),
                                 dataTableOutput("rf_e_metrics"),
                                 br(),
                                 h4("Best Model Configuration"),
                                 p('This is the best performing-model. The parameters are explained below:',
                                   style="font-size:18px"),
                                 HTML(
                                   '<ul>
                                   <li style = "font-size:18px"; >Trees refers to the number of trees used for fitting the model.</li>
                                   <li style = "font-size:18px"; >Min_n is the minimum number of observations taken for each model.</li>
                                   </ul>'),
                                 br(),
                                 dataTableOutput("rf_configuration"),
                                 br(),
                                 h3('Out-Of-Sample predictions'),
                                 br(),
                                 p("In this section you can see the results of training the model with original data
                                   and predicting the outcome with new data that the model hasn't seen. This is quite useful
                                   because when we update the models, we could have a lot of problems with the new data points
                                   and may waste a lot more of time. This can give you a better perspective about the new
                                   information given by the client. If the model", strong("crashes"), "(the previous statistics
                                   deteriorates a lot) the new data points could suggest a change in structure of the model,
                                   suggesting that it may be necessary remodelling; if not, it could just be a routine update.
                                   The blue vertical line shows the moment where the new data is taken",style="font-size:18px"),
                                 br(),
                                 plotlyOutput("rf_predictions_new"),
                                 br(),
                                 fluidRow(
                                   column(
                                     h3('Model Metrics'),
                                     br(),
                                     dataTableOutput("rf_new_m_metrics"),
                                     br(),
                                     width = 6,
                                   ),
                                   column(
                                     h3('Error-Based Metrics'),
                                     br(),
                                     dataTableOutput("rf_new_e_metrics"),
                                     br(),
                                     width = 6
                                   )
                                 ),
                                 br(),
                                 h3('Download your results'),
                                 p('Are you satisfied with the model? Download it!', style="font-size:18px"),
                                 downloadButton("download_rf_results", "Download Results")
                               )))

Tab5 <- tabPanel("Hierarchy Trees", fluid = T,
                 sidebarLayout(position = "left",
                               sidebarPanel(
                                 br(),
                                 selectInput(inputId = "drop_ht",
                                             label = "Drop variables for comparison:",
                                             "",
                                             multiple = T),
                                 p("Drop the variables you don't want to see in the comparison tree.
                                   You can drop as much as you want, the result is immediate."),
                                 width = size),
                               mainPanel(
                                 h3('Hierarchical Clustering'),
                                 br(),
                                 p('The idea behind this particular model, is the usage of some distance metric
                                   for capturing the simmilarity between data points, in this case, the similarity
                                   between variables. In this case, the technique is agglomerative, so each variable
                                   is a cluster and the minor distance is represented by how close the clusters are
                                   in the brances.',
                                   style="font-size:18px"),
                                 br(),
                                 p("This app encourages you to use the methodology in order to detect in a visual
                                   way the relations between variables (not only linear) and capture if some variables
                                   are more or less related to each other. You can choose in the left panel the variables
                                   you don't want to compare in the clustering; also you can choose the distance metric
                                   used and the method for clusterization",
                                   style="font-size:18px"),
                                 br(),
                                 h3('Select the parameters for your cluster'),
                                 br(),
                                 h4('Distances'),
                                 br(),
                                 HTML(
                                   '<ul>
                                   <li style = "font-size:15px"; >Euclidean distance is the ordinary distance between
                                                                  two points in the plane, taking the sum of the difference
                                                                  between the components.</li>
                                   <li style = "font-size:15px"; >Maximum is the maximum distance beetween the components
                                                                  of the points compared (supremum norm).</li>
                                   <li style = "font-size:15px"; >Manhattan is the Absolute distance between the two vectors.</li>
                                   <li style = "font-size:15px"; >Canberra is the absolute euclidian distance divided into the
                                                                  sum of the components.</li>
                                   <li style = "font-size:15px"; >Minkowski distance is the generalisation of the euclidean
                                                                  and manhattan distance. It takes the absolute difference
                                                                  between components to the p exponent depending on iterations.</li>
                                   </ul>'),
                                 br(),
                                 h4('Clustering'),
                                 br(),
                                 HTML(
                                   '<ul>
                                   <li style = "font-size:15px"; >Ward uses the minimum variance method, in order to create links
                                                                  that minimizes the intra-cluster variance.</li>
                                   <li style = "font-size:15px"; >Single computes all pairwise dissimilarities and considers the smallest
                                                                  one as a linkage criterion.</li>
                                   <li style = "font-size:15px"; >Complete compares element to element to identify the maximum distance beetween
                                                                  components and identify the minimum of these to create linkages.</li>
                                   <li style = "font-size:15px"; >Average uses the mean of the pairwise dissimilarities between components
                                                                  to create linkages, Median does the same operation.</li>
                                   <li style = "font-size:15px"; >Centroids uses the average of a vector X to compare to the average of another
                                                                  vector Y (of the same length) and creates linkages based on the minimum difference
                                                                  of both.</li>
                                   </ul>'),
                                 br(),
                                 splitLayout(
                                   selectInput(inputId = "distances_1",
                                               label = "Select the method for calculating distances.",
                                               choices = c(
                                                 "Euclidean" = "euclidean",
                                                 "Maximum"   = "maximum",
                                                 "Manhattan" = "manhattan",
                                                 "Canberra"  = "canberra",
                                                 "Minkowski" = "minkowsky"),
                                               selected = "euclidean"),
                                   selectInput(inputId = "cluster_1",
                                               label = "Select the method for clustering.",
                                               choices = c(
                                                 "Ward"      = "ward.D",
                                                 "Single"    = "single",
                                                 "Complete"  = "complete",
                                                 "Average"   = "average",
                                                 "Median"    = "median",
                                                 "Centroids" = "centroid"),
                                               selected = "average")),
                                 br(),
                                 tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                 plotOutput('single_dendo', height = "900px"),
                                 br(),
                                 h3('Want to compare to another clustering type?'),
                                 p('Sometimes you need to have a pair revision. In this case, you can compare
                                   your first hierarchical cluster with another one and check if the variables
                                   belong to the same end nodes. These can be compared with the inner links that
                                   match the variables. The usage is the same, select the distance and the cluster
                                   type',
                                   style="font-size:18px"),
                                 br(),
                                 splitLayout(
                                   selectInput(inputId = "distances_2",
                                               label = "Select the method for calculating distances.",
                                               choices = c(
                                                 "Euclidean" = "euclidean",
                                                 "Maximum"   = "maximum",
                                                 "Manhattan" = "manhattan",
                                                 "Canberra"  = "canberra",
                                                 "Binary"    = "binary",
                                                 "Minkowski" = "minkowsky"),
                                               selected = "euclidean"),
                                   selectInput(inputId = "cluster_2",
                                               label = "Select the method for clustering.",
                                               choices = c(
                                                 "Ward"      = "ward.D",
                                                 "Single"    = "single",
                                                 "Complete"  = "complete",
                                                 "Average"   = "average",
                                                 "McQuitty"  = "mcquitty",
                                                 "Median"    = "median",
                                                 "Centroids" = "centroid"),
                                               selected = "complete")),
                                 br(),
                                 plotOutput("tanglegram", height = "900px")
                               )))









