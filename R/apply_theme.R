#' Apply theme to a ggplot object
#'
#' @param ggobject A `ggplot2` object.
#' @param theme A string specifying the theme to be added to the ggplot2 plot. Possible choices are: `theme_base`, `theme_bw`, `theme_calc`, `theme_classic`, `theme_cowplot`, `theme_dark`, `theme_economist`, `theme_excel`, `theme_few`, `theme_fivethirtyeight`, `theme_gdocs`, `theme_grey`, `theme_hc`, `theme_igray`, `theme_light`, `theme_linedraw`, `theme_minimal`, `theme_pander`, `theme_solarized`, `theme_stata_s2color`, `theme_stata_s2mono`, `theme_stata_s1color`, `theme_stata_s1rcolor`, `theme_stata_s1mono`, `theme_stata_s2manual`, `theme_stata_s1manual`, `theme_stata_sj`, `theme_tufte`, `theme_wsj`. Themes, color scales, and fill scales are imported from the `ggplot2`, `ggthemes`, and `cowplot` packages.
#' @return A `ggplot2` object.
apply_theme <- function(ggobject, theme) {
  th <- switch(
    theme,
    "theme_base" = ggthemes::theme_base(),
    "theme_bw" = ggplot2::theme_bw(),
    "theme_calc" = ggthemes::theme_calc(),
    "theme_classic" = ggplot2::theme_classic(),
    "theme_cowplot" = cowplot::theme_cowplot(),
    "theme_dark" = ggplot2::theme_dark(),
    "theme_economist" = ggthemes::theme_economist(),
    "theme_excel" = ggthemes::theme_excel(),
    "theme_few" = ggthemes::theme_few(),
    "theme_fivethirtyeight" = ggthemes::theme_fivethirtyeight(),
    "theme_gdocs" = ggthemes::theme_gdocs(),
    "theme_grey" = ggplot2::theme_grey(),
    "theme_hc" = ggthemes::theme_hc(),
    "theme_igray" = ggthemes::theme_igray(),
    "theme_light" = ggplot2::theme_light(),
    "theme_linedraw" = ggplot2::theme_linedraw(),
    "theme_minimal" = ggplot2::theme_minimal(),
    "theme_pander" = ggthemes::theme_pander(),
    "theme_solarized" = ggthemes::theme_solarized(),
    "theme_stata_s2color" = ggthemes::theme_stata(scheme = "s2color"),
    "theme_stata_s2mono" = ggthemes::theme_stata(scheme = "s2mono"),
    "theme_stata_s1color" = ggthemes::theme_stata(scheme = "s1color"),
    "theme_stata_s1rcolor" = ggthemes::theme_stata(scheme = "s1rcolor"),
    "theme_stata_s1mono" = ggthemes::theme_stata(scheme = "s1mono"),
    "theme_stata_s2manual" = ggthemes::theme_stata(scheme = "s2manual"),
    "theme_stata_s1manual" = ggthemes::theme_stata(scheme = "s1manual"),
    "theme_stata_sj" = ggthemes::theme_stata(scheme = "sj"),
    "theme_tufte" = ggthemes::theme_tufte(),
    "theme_wsj" = ggthemes::theme_wsj()
  )
  return(ggobject + th)
}
