
#' Sort cards in a Shuffle grid layout
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#' @param sort_by Key(s) defined in \code{shuffle_card} to sort elements.
#' @param numeric Logical, set to \code{TRUE} if key is a numeric value.
#' @param decreasing Logical, set to \code{TRUE} to sort in decreasing order.
#'
#' @export
#'
#' @name sort-cards
#'
#' @examples
#'
#' # TODO
sort_cards <- function(session, shuffleId, sort_by, numeric = FALSE, decreasing = FALSE) {
  message <- list(type = "sort", sortBy = sort_by, numeric = numeric, decreasing = decreasing, random = FALSE)
  session$sendInputMessage(shuffleId, message)
}

#' @export
#' @rdname sort-cards
randomize_cards <- function(session, shuffleId) {
  message <- list(type = "sort", random = TRUE)
  session$sendInputMessage(shuffleId, message)
}


#' Filter a Shuffle grid layout by groups
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#' @param groups Groups (defined in \code{shuffle_card}) you want to display.
#'
#' @export
#'
#' @examples
#'
#' # TODO
filter_cards_groups <- function(session, shuffleId, groups) {
  if (length(groups) == 0)
    groups <- as.character(sample.int(1e6, 1))
  message <- list(type = "filter-groups", groups = groups)
  session$sendInputMessage(shuffleId, message)
}



#' Filter a Shuffle grid layout with custom values
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#' @param filter_by Key defined in \code{shuffle_card} to filter elements, can be \code{"id"}
#'  to refer to the unique ID associated with the card.
#' @param filter_list A named list, where names match the names used in \code{filter_by}
#'  for \code{shuffle_card} and value are \code{TRUE} or \code{FALSE}. Alternatively a \code{data.frame}
#'  where first columns contains names and second \code{TRUE} or \code{FALSE}.
#'
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' # TODO
filter_cards <- function(session, shuffleId, filter_by, filter_list) {
  stopifnot(is.character(filter_by) & length(filter_by) == 1)
  if (is.data.frame(filter_list)) {
    filter_list <- setNames(as.list(filter_list[[2]]), filter_list[[1]])
  } else {
    filter_list <- as.list(filter_list)
    if (!all(nzchar(names(filter_list)))) {
      stop("'filter_list' must be named !", call. = FALSE)
    }
  }
  filter_list <- lapply(
    X = setNames(filter_list, names(filter_list)),
    FUN = function(x) {
      if (length(x) > 1) {
        warning("'filter_list' elements must be of length one, keeping first one", call. = FALSE)
        x <- x[1]
      }
      if (is.na(x)) {
        warning("'filter_list' doesn't support missing values, treating as FALSE", call. = FALSE)
        x <- FALSE
      }
      if (!is.logical(x)) {
        stop("'filter_list' elements must be TRUE or FALSE", call. = FALSE)
      }
      x
    }
  )
  if (filter_by != "id") {
    filter_by <- paste("data", filter_by, sep = "-")
  }
  message <- list(type = "filter-custom", filterBy = filter_by, filterList = filter_list)
  session$sendInputMessage(shuffleId, message)
}



#' Update Shuffle Instance
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#'
#' @export
#'
#' @examples
#' # TODO
update_shuffle <- function(session, shuffleId) {
  session$sendInputMessage(shuffleId, list(type = "update"))
}



