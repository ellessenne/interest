#' interest
#'
#' @title INTEREST
#' @description Run the INTEREST interactive shiny app.
#' @param ... Passed onto [shiny::runApp()].
#' @export
#' @importFrom rlang "!!"
#'
#' @examples
#' \dontrun{
#' library(interest)
#' interest()
#' }
interest <- function(...) {
  shiny::runApp(appDir = system.file("application", package = "interest"), ...)
}
