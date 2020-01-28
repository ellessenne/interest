# make MIsim_big
set.seed(923648)
library(tidyverse)
library(usethis)
data("MIsim", package = "rsimsum")
MIsim_list <- lapply(X = seq(200), FUN = function(i) dplyr::mutate(MIsim, artrep = i))
MIsim_big <- dplyr::bind_rows(MIsim_list)
MIsim_big <- dplyr::mutate(MIsim_big, i = dataset * artrep)
MIsim_big$b <- rnorm(n = nrow(MIsim_big), mean = MIsim_big$b, sd = 0.01)
MIsim_big$se <- rnorm(n = nrow(MIsim_big), mean = MIsim_big$se, sd = 0.01)
# Save
usethis::use_data(MIsim_big, overwrite = TRUE)
