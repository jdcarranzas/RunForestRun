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
require(shinydashboard)
require(shiny)
require(randomForest)


# Define UI for application that draws a histogram

#source('tabs.R')

ui <- shinyUI({
  fluidPage(titlePanel('Alternate Ways'),
    tabsetPanel(type = "tabs",
      Tab1,
      Tab2,
      Tab3,
      Tab4,
      Tab5
    )
  )
})

