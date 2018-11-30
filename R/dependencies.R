
#' @importFrom htmltools htmlDependency
shuffle_dependencies <- function() {
  htmlDependency(
    name = "shufflejs", version = "5.2.0",
    src = list(href = "shufflejs", file = "shufflejs"), package = "shufflecards",
    script = c("shuffle.min.js", "shuffle-bindings.js"),
    stylesheet = "shuffle-custom.css"
  )
}

#' @importFrom htmltools htmlDependency
playing_cards_dependencies <- function() {
  htmlDependency(
    name = "css-playing-cards", version = "2.0",
    src = list(href = "playing-cards", file = "css-playing-cards"), package = "shufflecards",
    stylesheet = "cards.css"
  )
}

#' @importFrom htmltools htmlDependency
polyfill_dependencies <- function() {
  htmlDependency(
    name = "polyfill",
    version = "2.0",
    src = list(href = "https://cdn.polyfill.io/v2/polyfill.min.js", file = "polyfill"),
    script = "inject-polyfill.js",
    package = "shufflecards"
  )
}
