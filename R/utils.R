
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

validate_card <- function(card) {
  if (!"shufflecard.tag" %in% class(card)) {
    stop("Card element must be created with function `shuffle_card`", call. = FALSE)
  }
}

validate_cards <- function(cards) {
  if (length(cards) > 0) {
    lapply(cards, validate_card)
  }
}

#' @importFrom htmltools htmlDependencies<-
removeDependencies <- function(tag) {
  htmlDependencies(tag) <- NULL
  tag
}

