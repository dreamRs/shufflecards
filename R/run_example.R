
#' Run examples of use of 'shufflecards'
#'
#' @param example Name of the example, between : \code{"gapminder"}, \code{"bs-grid"}, \code{"grid-plot"}.
#'
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples
#' if (interactive()) {
#'
#' run_example("gapminder")
#'
#' }
run_example <- function(example = c("gapminder", "bs-grid", "grid-plot", "filter-sort")) {
  example <- match.arg(example)
  path <- file.path("examples", paste0(example, "-app"))
  runApp(
    appDir = system.file(path, package = "shufflecards", mustWork = TRUE),
    display.mode = "showcase"
  )
}
