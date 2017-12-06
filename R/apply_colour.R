#' Apply colour to a ggplot object
#'
#' @param ggobject A `ggplot2` object.
#' @param theme A string specifying the colour and fill scales to be added to the ggplot2 plot. Possible choices are: `theme_base`, `theme_bw`, `theme_calc`, `theme_classic`, `theme_cowplot`, `theme_dark`, `theme_economist`, `theme_excel`, `theme_few`, `theme_fivethirtyeight`, `theme_gdocs`, `theme_grey`, `theme_hc`, `theme_igray`, `theme_light`, `theme_linedraw`, `theme_minimal`, `theme_pander`, `theme_solarized`, `theme_stata_s2color`, `theme_stata_s2mono`, `theme_stata_s1color`, `theme_stata_s1rcolor`, `theme_stata_s1mono`, `theme_stata_s2manual`, `theme_stata_s1manual`, `theme_stata_sj`, `theme_tufte`, `theme_wsj`. Color and fill scales are imported from the `ggplot2`, `ggthemes`, and `cowplot` packages.
#' @return A `ggplot2` object.
#' @export
apply_colour = function(ggobject, theme) {
	sc = switch(
		theme,
#		"theme_base" = ,
#		"theme_bw" = ,
		"theme_calc" = ggthemes::scale_fill_calc(),
#		"theme_classic" = ,
#		"theme_cowplot" = ,
#		"theme_dark" = ,
		"theme_economist" = ggthemes::scale_color_economist(),
		"theme_excel" = ggthemes::scale_fill_excel(),
		"theme_few" = ggthemes::scale_color_few(),
		"theme_fivethirtyeight" = ggthemes::scale_colour_fivethirtyeight(),
		"theme_gdocs" = ggthemes::scale_fill_gdocs(),
#		"theme_grey" = ,
		"theme_hc" = ggthemes::scale_colour_hc(),
#		"theme_igray" = ,
#		"theme_light" = ,
#		"theme_linedraw" = ,
#		"theme_minimal" = ,
		"theme_pander" = ggthemes::scale_color_pander(),
		"theme_solarized" = ggthemes::scale_fill_solarized(),
		"theme_stata_s2color" = ggthemes::scale_color_stata(scheme = "s2color"),
		"theme_stata_s2mono" = ggthemes::scale_colour_stata(scheme = "s2mono"),
		"theme_stata_s1color" = ggthemes::scale_colour_stata(scheme = "s1color"),
		"theme_stata_s1rcolor" = ggthemes::scale_colour_stata(scheme = "s1rcolor"),
		"theme_stata_s1mono" = ggthemes::scale_colour_stata(scheme = "s1mono"),
		"theme_stata_s2manual" = ggthemes::scale_colour_stata(scheme = "s2manual"),
		"theme_stata_s1manual" = ggthemes::scale_colour_stata(scheme = "s1manual"),
		"theme_stata_sj" = ggthemes::scale_colour_stata(scheme = "sj"),
#		"theme_tufte" = ,
		"theme_wsj" = ggthemes::scale_colour_wsj()
	)
	return(ggobject + sc)
}
