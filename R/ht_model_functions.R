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


# 1. Preprocess data ------------------------------------------------------

preprocess_hc <- function(dataset, dist_method, hc_method){
  preproc <- dataset %>%
    select_if(is.numeric) %>%
    select_if(colSums(.)!=0) %>%
    mutate(across(.cols = everything(), ~scale(.x))) %>%
    t() %>%
    dist(method = dist_method) %>%
    hclust(method = hc_method) %>%
    as.dendrogram()
  return(preproc)
}


# 2. Plot the unique dendogram --------------------------------------------

plot_dendogram <- function(dendo){
  dendo %>%
    set('labels_col', value = c('red', 'blue', 'green', 'gray', 'orange', 'purple'), k = 6) %>%
    set('branches_k_color', value = c('red', 'blue', 'green', 'gray', 'orange', 'purple'), k = 6) %>%
    set('labels_cex', 0.7) %>%
    as.ggdend() %>%
    ggplot(horiz = T, theme = theme_classic())+
      geom_text(check_overlap = TRUE)

}


# 3. Plot tanglegram ------------------------------------------------------

plot_tanglegram <- function(dendo_1, dendo_2, name_dist_1, name_clust_1, name_dist_2, name_clust_2){
  dendlist(
    dendo_1 %>%
      set("labels_col", value = c('red', 'blue', 'green', 'gray', 'orange', 'purple'), k = 6) %>%
      set('branches_k_color', value = c('red', 'blue', 'green', 'gray', 'orange', 'purple'), k = 6) %>%
      set("branches_lty", 1),
    dendo_2 %>%
      set("labels_col", value = c('red', 'blue', 'green', 'gray', 'orange', 'purple'), k = 6) %>%
      set('branches_k_color', value = c('red', 'blue', 'green', 'gray', 'orange', 'purple'), k = 6) %>%
      set("branches_lty", 2)
  ) %>%
  tanglegram(
      common_subtrees_color_lines = TRUE,
      highlight_distinct_edges  = FALSE,
      highlight_branches_lwd = FALSE,
      edge.lwd=2,
      columns_width = c(5, 3, 5),
      margin_inner=14,
      margin_outer=4,
      lwd=2,
      axes = F,
      main_left = paste0("Dist: ", name_dist_1,
                         "; Clust:", name_clust_1),
      main_right = paste0("Dist: ", name_dist_2,
                          "; Clust:", name_clust_2),
      lab.cex = 1.3
    )

}
