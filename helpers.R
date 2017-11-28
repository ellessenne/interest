## common.R script ##

# Required packages
if (!requireNamespace("devtools")) install.packages("devtools")
if (!requireNamespace("rsimsum")) devtools::install_github("ellessenne/rsimsum")
pacman::p_load("tidyverse", "shiny", "shinydashboard")
