
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

snake_to_camel <- function(x) {
  gsub(pattern = "_([a-z])", replacement = "\\U\\1", x = x, perl = TRUE)
}

get_eval <- function(x) {
  x <- vapply(x, function(x) is.character(x) && inherits(x, 'AsIs'), logical(1))
  names(x)[x]
}
