# make MIsim_big
library(rsimsum)
library(dplyr)
data("tt", package = "rsimsum")
# See ?tt
tt <- tt %>%
  dplyr::mutate(dgm = factor(dgm, levels = seq(4), labels = c("Symmetric, equal variance", "Symmetric, unequal variance", "Asymmetric, equal variance", "Aymmetric, unequal variance"))) %>%
  dplyr::mutate(method = factor(method, levels = seq(2), labels = c("t-test (pooled)", "t-test (unpooled)"))) %>%
  dplyr::mutate(true = -1)
# Save for app
saveRDS(object = tt, file = "inst/application/data/tt.RDS")
