

#' Create a Shuffle container
#'
#' @param shuffleId Shuffle's id.
#' @param ... List of \code{shuffle_card}s to include.
#' @param card_list Alternative list of \code{shuffle_card}s to include.
#' @param no_card UI definition (or text) to display when all cards are filtered out.
#' @param width The width of the container, e.g. \code{'400px'}, or \code{'100\%'}; see \code{\link[htmltools]{validateCssUnit}}.
#'
#' @export
#'
#' @importFrom htmltools tags tagList attachDependencies tagAppendAttributes validateCssUnit
#'
#' @examples
#'
#' # TODO
shuffle_container <- function(shuffleId, ..., card_list = NULL, no_card = NULL, width = NULL) {
  args <- list(...)
  nargs <- names(args)
  if (is.null(nargs))
    nargs <- rep_len("", length(args))
  cards <- c(args[nzchar(nargs) == 0], card_list)
  validate_cards(cards)
  args <- args[nzchar(nargs) > 0]
  shuffleTag <- tags$div(
    id = shuffleId, class = "shuffle-container",
    style = if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";"),
    tagList(cards),
    tags$div(class = paste("col-1@sm", paste0(shuffleId, "-sizer-element")))
  )
  shuffleTag <- do.call(tagAppendAttributes, c(list(tag = shuffleTag), args))
  tagList(
    attachDependencies(shuffleTag, shuffle_dependencies()),
    tags$div(no_card, id = paste0(shuffleId, "-nodata"), style = "display: none;", class = "shuffle-nodata"),
    init_md(shuffleId)
  )
}




#' Shuffle card element
#'
#' @param ... UI elements to include within the card.
#' @param groups Character vector of groups used to filtering.
#' @param id Cards's id.
#' @param class CSS class(es) to apply on the card.
#' @param style Inline CSS to apply on the card.
#' @param width The width of the container, e.g. \code{'400px'}, or \code{'100\%'}; see \code{\link[htmltools]{validateCssUnit}}.
#'
#' @export
#'
#' @importFrom htmltools tag tagAppendAttributes validateCssUnit
#' @importFrom jsonlite toJSON
#'
#' @examples
#'
#' # TODO
shuffle_card <- function(..., groups = NULL, id = NULL, class = NULL, style = NULL, width = NULL) {
  args <- list(...)
  nargs <- names(args)
  has_names <- nzchar(nargs)
  if (length(has_names) > 0) {
    names(args)[has_names] <- paste0("data-", names(args)[has_names])
  }
  tag_el <- tag("div", args)
  tag_attributes <- dropNulls(list(
    id = id, class = class, class = "element-item", style = style,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    `data-groups` = toJSON(as.character(groups)),
    style = "margin: 5px;"
  ))
  tag_el <- do.call(tagAppendAttributes, c(list(tag = tag_el), tag_attributes))
  class(tag_el) <- c(class(tag_el), "shufflecard.tag")
  return(tag_el)
}


