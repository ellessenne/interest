#' Apply theme to a ggplot object
#'
#' @param ggobject A `ggplot2` object.
#' @param theme A string specifying the theme to be added to the ggplot2 plot. Possible choices are: `theme_bw`, `theme_dark`, `theme_few`, `theme_grey`, `theme_light`, `theme_linedraw`, `theme_minimal`, `theme_tufte`, `theme_viridis`. Themes, color scales, and fill scales are imported from the `ggplot2`, `ggthemes`, `viridis`
#' @return A `ggplot2` object.
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
