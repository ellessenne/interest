#' @title Get example data
#' @description Copy an example dataset to use with INTEREST to a given directory.
#'
#' @param dataset The dataset to copy. Possible choices are: `MIsim`, `relhaz`, `frailty`. More information on each dataset can be obtained by typing `?interest::MIsim`, `?interest::relhaz`, or `?interest::frailty` in the R console.
#' @param where Where to copy the dataset. Defaults to the current directory.
#'
#' @return The required dataset, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' get_example_data("MIsim")
#' }
get_example_data <- function(dataset = c("MIsim", "relhaz", "frailty"), where = ".") {
  dataset <- match.arg(dataset)
  utils::data(list = dataset, package = "interest", envir = environment())
  saveRDS(object = get(dataset), file = paste0(where, "/", dataset, ".rds"))
  invisible(get(dataset))
}
