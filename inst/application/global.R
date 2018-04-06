## global.R script ##

SummaryStatistics <- c("Simulations with non-missing estimates/SEs" = "nsim", "Average point estimate" = "thetamean", "Median point estimate" = "thetamedian", "Average standard error" = "se2mean", "Median standard error" = "se2median", "Bias in point estimate" = "bias", "Empirical standard error" = "empse", "Mean squared error" = "mse", "% gain in precision relative to reference method" = "relprec", "Model-based standard error" = "modelse", "Relative % error in standard error" = "relerror", "Coverage of nominal 95% CI" = "cover", "Bias corrected coverage of nominal 95% CI" = "bccover", "Power of 5% level test" = "power")

# apply_* functions are here as long as the interest package is not on CRAN
apply_colour <- function(ggobject, theme) {
	return(
		switch(
			theme,
			"theme_bw" = ggobject,
			"theme_dark" = ggobject,
			"theme_few" = ggobject + ggthemes::scale_color_few(),
			"theme_grey" = ggobject,
			"theme_light" = ggobject,
			"theme_linedraw" = ggobject,
			"theme_minimal" = ggobject,
			"theme_tufte" = ggobject,
			"theme_viridis" = ggobject + viridis::scale_color_viridis() + viridis::scale_fill_viridis()
		)
	)
}
apply_theme <- function(ggobject, theme) {
	return(
		switch(
			theme,
			"theme_bw" = ggobject + ggplot2::theme_bw(),
			"theme_dark" = ggobject + ggplot2::theme_dark(),
			"theme_few" = ggobject + ggthemes::theme_few(),
			"theme_grey" = ggobject + ggplot2::theme_grey(),
			"theme_light" = ggobject + ggplot2::theme_light(),
			"theme_linedraw" = ggobject + ggplot2::theme_linedraw(),
			"theme_minimal" = ggobject + ggplot2::theme_minimal(),
			"theme_tufte" = ggobject + ggthemes::theme_tufte(),
			"theme_viridis" = ggobject
		)
	)
}
