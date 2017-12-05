#' interest
#'
#' @title INTEREST
#' @description Run the INTEREST interactive shiny app.
#' @param ... Passed onto [shiny::runApp()].
#' @export
#'
#' @examples
#' interest()
interest <- function(...)
{
	shiny::runApp(appDir = system.file("application", package = "interest"), ...)
}
