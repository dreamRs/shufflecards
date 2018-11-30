
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

validate_card <- function(card, shuffleId = NULL) {
  if (!"shufflecard.tag" %in% class(card)) {
    stop("Card element must be created with function `shuffle_card`", call. = FALSE)
  }
  if (!is.null(shuffleId)) {
    card <- tagAppendAttributes(card, `data-shuffleId` = shuffleId)
  }
  card
}

validate_cards <- function(cards, shuffleId = NULL) {
  lapply(cards, validate_card, shuffleId = shuffleId)
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
