## common.R script ##

if (!requireNamespace("shiny")) install.packages("shiny")
if (!requireNamespace("shinydashboard")) install.packages("shinydashboard")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("tidyr")) install.packages("tidyr")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("xtable")) install.packages("xtable")
if (!requireNamespace("readr")) install.packages("readr")

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(readr)

# generate data
# set.seed(1)
# sample_data = expand.grid(ni = c(100, 500, 1000),
# 													nc = c(10, 25, 50),
# 													method = sample(LETTERS, size = 3),
# 													par = sample(letters, size = 5),
# 													true_value = c(-0.5, 0.0, 0.5, 1.0, 1.5, 2.0),
# 													row = 1:100)
# sample_data$value = sapply(sample_data$true_value, function(x) rnorm(1, mean = x, sd = 2 * abs(x) ^ x))
# sample_data$se = sapply(sample_data$true_value, function(x) runif(1, 0, abs(x) * abs(x)))
# sample_data$row = NULL
# readr::write_csv(sample_data, path = "data/sample.csv")
