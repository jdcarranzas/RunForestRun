#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages_for_install <- c("skimr",
                          "tidyverse",
                          "modeltime",
                          "tidymodels",
                          "workflowsets",
                          "timetk",
                          "xgboost",
                          "randomForest",
                          "caret",
                          "lubridate",
                          "devtools",
                          "plotly",
                          "openxlsx",
                          "writexl")

install.packages(setdiff(packages_for_install,
                         rownames(installed.packages())),
                 repos = "http://cran.us.r-project.org")


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


executeApp <- function(){
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

  options(scipen = 999,
          digits = 2)

  modeltime::parallel_start(4, .method = "parallel")

  #source('ui.R', local = TRUE)
  #source('server.R')

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}



