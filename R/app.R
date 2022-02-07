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
                          "writexl",
                          "dendextend")

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

#source("R/server.R")
#source("R/user_interface.R")

executeApp <- function(cores = 8){
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
                            "writexl",
                            "dendextend")

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
  require(randomForest)
  require(dendextend)

  options(scipen = 999,
          digits = 2)

  modeltime::parallel_start(cores, .method = "parallel")

  # Run the application
  shiny::shinyApp(ui = user_interface, server = server)
}



