#' Apply colour to a ggplot object
#'
#' @param ggobject A `ggplot2` object.
#' @param theme A string specifying the theme to be added to the ggplot2 plot. Possible choices are: `theme_bw`, `theme_dark`, `theme_few`, `theme_grey`, `theme_light`, `theme_linedraw`, `theme_minimal`, `theme_tufte`, `theme_viridis`. Themes, color scales, and fill scales are imported from the `ggplot2`, `ggthemes`, `viridis`.
#' @return A `ggplot2` object.
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

#' @rdname apply_colour
apply_color <- function(ggobject, theme) {
  apply_colour(ggobject = ggobject, theme = theme)
}
