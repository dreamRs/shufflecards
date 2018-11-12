
#' Group of buttons for Markdown documents
#'
#' @param shuffleId The id of the shuffle container.
#' @param ... \code{\link{sort_button}} buttons to include.
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


#' Buttons for markdown document
#'
#' @param label The contents of the button.
#' @param sort_by Key(s) defined in \code{shuffle_card} to sort elements.
#' @param numeric Logical, set to \code{TRUE} if key is a numeric value.
#' @param decreasing Logical, set to \code{TRUE} to sort in decreasing order.
#' @param status Add a class to the buttons, you can use Bootstrap status like 'info',
#'  'primary', 'danger', 'warning' or 'success'. Or use an arbitrary strings to add a
#'  custom class, e.g. : with \code{status = 'myClass'}, buttons will have class \code{btn-myClass}.
#' @param icon An optional icon to appear on the button.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'}.
#' @param ... Named attributes to be applied to the button.
#'
#' @importFrom htmltools validateCssUnit tags
#' @importFrom rmarkdown html_dependency_font_awesome
#' @export
#'
#'
#' @examples
#' # TODO
sort_button <- function(label, sort_by, numeric = FALSE, decreasing = FALSE, status = "default", icon = NULL, width = NULL, ...) {
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
    `data-sort-by` = sort_by,
    `data-sort-numeric` = tolower(numeric),
    `data-sort-decreasing` = tolower(decreasing),
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






