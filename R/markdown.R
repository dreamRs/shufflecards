
#' Group of buttons for Markdown documents
#'
#' @param shuffleId The id of the shuffle container.
#' @param ... \code{\link{arrange_button}} buttons to include.
#' @param label Optionnal label to display above buttons.
#'
#' @export
#' @importFrom htmltools tagList tags
#'
#'
#' @examples
#' # TODO
rmd_group_buttons <- function(shuffleId, ..., label = NULL) {
  tags$div(
    class = paste0("button-group sort-shuffle-btn-", shuffleId),
    if (!is.null(label)) tagList(tags$label(label), tags$br()),
    ...
  )
}


#' @title Buttons for markdown document
#'
#' @description Arrange a Shuffle grid in markdown
#'
#' @param label The contents of the button.
#' @param by Key(s) defined in \code{shuffle_card} to sort elements.
#' @param numeric Logical, set to \code{TRUE} if key is a numeric value.
#' @param desc Logical, set to \code{TRUE} to sort in decreasing order.
#' @param status Add a class to the buttons, you can use Bootstrap status like 'info',
#'  'primary', 'danger', 'warning' or 'success'. Or use an arbitrary strings to add a
#'  custom class, e.g. : with \code{status = 'myClass'}, buttons will have class \code{btn-myClass}.
#' @param icon An optional icon to appear on the button.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'}.
#' @param ... Named attributes to be applied to the button.
#'
#' @note Use \code{\link{rmd_group_buttons}} to position several buttons.
#'
#' @importFrom htmltools validateCssUnit tags
#' @importFrom rmarkdown html_dependency_font_awesome
#' @export
#'
arrange_button <- function(label, by, numeric = FALSE, desc = FALSE, status = "default", icon = NULL, width = NULL, ...) {
  if (!is.null(icon)) {
    if ("shiny.tag" %in% class(icon)) {
      icon <- removeDependencies(icon)
    }
  }
  tags$button(
    style = if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";"),
    type = "button",
    class = paste0("btn btn-", status),
    `data-sort-by` = by,
    `data-sort-numeric` = tolower(numeric),
    `data-sort-decreasing` = tolower(desc),
    list(icon, label), ...,
    if (!is.null(icon)) html_dependency_font_awesome()
  )
}


# # @param groups Groups (defined in \code{shuffle_card}) you want to display.
# filter_button <- function(label, groups, status = "default", icon = NULL, width = NULL, ...) {
#   if (!is.null(icon)) {
#     if ("shiny.tag" %in% class(icon)) {
#       icon <- removeDependencies(icon)
#     }
#   }
#   tags$button(
#     style = if (!is.null(width))
#       paste0("width: ", validateCssUnit(width), ";"),
#     type = "button",
#     class = paste0("btn btn-", status),
#     `data-filter-groups` = toJSON(groups),
#     list(icon, label), ...,
#     if (!is.null(icon)) html_dependency_font_awesome()
#   )
# }
#



#' @importFrom htmltools HTML tags
init_md <- function(shuffleId) {
  tags$script(HTML( paste(
    "if (!(typeof(window.Shiny) !== 'undefined' && !!window.Shiny.outputBindings)) {",
    "document.addEventListener('DOMContentLoaded', function () {",
    paste0("window.shuffle = new shuffling(document.getElementById(\"", shuffleId, "\"));"),
    "});", "}", collapse = "\n"
  )))
}



#' Update Shuffle Layout in Markdown documentd
#'
#' Use this if you have Shuffle layout in tab panels in a markdown document.
#'
#' @param shuffleId The id of the shuffle container.
#' @param selector Selector to trigger update, typically a button,
#' you can use \href{https://selectorgadget.com/}{SelectorGadget} to find the proper selector.
#'
#' @export
#'
#' @importFrom htmltools tags
#'
md_shuffle_update <- function(shuffleId, selector) {
  js <- paste0()
  tags$script(HTML(paste(
    "document.addEventListener('DOMContentLoaded', function () {",
    paste0("var trigger = document.querySelector('", selector, "');"),
    "trigger.addEventListener('click', function() {",
    paste0("var shuffleObj = document.querySelector('#", shuffleId, "');"),
    "shuffleObj.dispatchEvent(shufflecardsupdate);",
    "});",
    "});"
    , collapse = "\n")))
}




#' Convert a `ggplot2`  object to `ggiraph::girafe`
#'
#' \code{\link{shuffle_widget}} generates HTML tags, to represent `ggplot2` objects you need to convert them.
#' This is the purpose of this function, you can also use `ggiraph` or `plotly`.
#'
#' @param p A `ggplot2` object.
#' @param width Width, in numeric and pixels.
#' @param height Height, in numeric and pixels.
#'
#' @export
#'
#' @importFrom ggiraph girafe
#' @importFrom htmltools HTML validateCssUnit
#'
as_girafe <- function(p, width = 400, height = 300) {
  if (!inherits(p, what = "ggplot")) {
    stop("'p' must be of class 'ggplot'", call. = FALSE)
  }
  if (!is.numeric(width) | !is.numeric(height)) {
    stop("'width' & 'height' must be numeric (in pixels)", call. = FALSE)
  }
  tags$div(
    style = if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";"),
    style = if (!is.null(height))
      paste0("height: ", validateCssUnit(height), ";"),
    # HTML(stringSVG(print(p), width = width/0.75/72, height = height/0.75/72, standalone = FALSE))
    ggiraph::girafe(print(p), width_svg = width/0.75/72, height_svg = height/0.75/72)
  )
}
