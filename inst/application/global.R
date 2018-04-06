## global.R script ##

SummaryStatistics <- c("Simulations with non-missing estimates/SEs" = "nsim", "Average point estimate" = "thetamean", "Median point estimate" = "thetamedian", "Average standard error" = "se2mean", "Median standard error" = "se2median", "Bias in point estimate" = "bias", "Empirical standard error" = "empse", "Mean squared error" = "mse", "% gain in precision relative to reference method" = "relprec", "Model-based standard error" = "modelse", "Relative % error in standard error" = "relerror", "Coverage of nominal 95% CI" = "cover", "Bias corrected coverage of nominal 95% CI" = "bccover", "Power of 5% level test" = "power")

# Install INTEREST if not available
if (!require(devtools)) install.packages("devtools")
if (!require(interest) | (require(interest) & utils::packageVersion("interest") != "0.0.1.9003")) devtools::install_github("ellessenne/interest")
