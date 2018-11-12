#' Shiny resource
#'
#' @importFrom shiny addResourcePath registerInputHandler
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("shufflejs", system.file("shufflejs", package = "shufflecards"))
  shiny::addResourcePath("playing-cards", system.file("css-playing-cards", package = "shufflecards"))
}
