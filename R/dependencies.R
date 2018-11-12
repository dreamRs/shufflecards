
#' @importFrom htmltools htmlDependency
shuffle_dependencies <- function() {
  htmlDependency(
    name = "shufflejs", version = "5.2.0",
    src = c(href = "shufflejs", file = "shufflejs"), package = "shufflecards",
    script = c("shuffle.min.js", "shuffle-bindings.js"),
    stylesheet = "shuffle-custom.css"
  )
}

#' @importFrom htmltools htmlDependency
playing_cards_dependencies <- function() {
  htmlDependency(
    name = "css-playing-cards", version = "2.0",
    src = c(href = "playing-cards", file = "css-playing-cards"), package = "shufflecards",
    stylesheet = "cards.css"
  )
}
