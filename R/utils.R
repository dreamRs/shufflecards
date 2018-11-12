
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

validate_cards <- function(cards) {
  if (length(cards) > 0) {
    lapply(cards, function(x) {
      if (!"shufflecard.tag" %in% class(x)) {
        stop("Cards element must be created with function `shuffle_card`", call. = FALSE)
      }
    })
  }
}

#' @importFrom htmltools htmlDependencies<-
removeDependencies <- function(tag) {
  htmlDependencies(tag) <- NULL
  tag
}

