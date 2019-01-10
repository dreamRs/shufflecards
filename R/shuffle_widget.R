#' Htmlwidget for 'Shuffle.js'
#'
#' Create a grid layout in markdown. Arrange the grid with \code{\link{arrange_button}}s and filter it with crosstalk inputs.
#'
#' @param ... List of \code{shuffle_card}s to include.
#' @param card_list Alternative list of \code{shuffle_card}s to include.
#' @param shared_data A `crosstalk` \code{\link[crosstalk]{SharedData}} object
#'  where a row represent a \code{\link{shuffle_card}} and can be used for filter the grid.
#' @param options Options for Shuffle, see \code{\link{shuffle_options}}.
#' @param no_card UI definition (or text) to display when all cards are filtered out.
#' @param width A numeric input in pixels.
#' @param height A numeric input in pixels.
#' @param shuffleId Use an explicit element ID for the widget.
#'  Needed to associate the widget to \code{\link{rmd_group_buttons}}.
#'
#'
#' @importFrom htmlwidgets createWidget sizingPolicy
#' @importFrom htmltools renderTags
#' @importFrom crosstalk is.SharedData crosstalkLibs
#'
#' @export
shuffle_widget <- function(..., card_list = NULL, shared_data = NULL, options = shuffle_options(),
                           no_card = NULL, width = NULL, height = NULL, shuffleId = NULL) {

  if (!inherits(options, "shuffle.options"))
    stop("'options' must be generated with 'shuffle_options'", call. = FALSE)
  args <- list(...)
  nargs <- names(args)
  if (is.null(nargs))
    nargs <- rep_len("", length(args))
  cards <- c(args[nzchar(nargs) == 0], card_list)
  cards <- validate_cards(cards, shuffleId)
  args <- args[nzchar(nargs) > 0]

  options$options$itemSelector <- ".element-item"

  rendered_tags <- renderTags(x = cards)
  rendered_nocard <- doRenderTags(no_card)
  deps <- rendered_tags$dependencies

  if (!is.null(shared_data) && is.SharedData(shared_data)) {
    key <- shared_data$key()
    group <- shared_data$groupName()
    shared_data <- shared_data$origData()
    deps <- c(deps, crosstalkLibs())
  } else {
    key <- NULL
    group <- NULL
  }

  x = list(
    shared_data = shared_data,
    html = rendered_tags$html,
    options = options,
    nocard = rendered_nocard,
    settings = list(
      crosstalk_key = key,
      crosstalk_group = group
    )
  )



  # create widget
  createWidget(
    name = 'shuffle_widget',
    x = x,
    dependencies = deps,
    width = width,
    height = height,
    package = 'shufflecards',
    elementId = shuffleId,
    sizingPolicy = sizingPolicy(
      defaultWidth = "95%",
      viewer.defaultHeight = "100%",
      viewer.defaultWidth = "100%",
      knitr.figure = FALSE,
      viewer.suppress = TRUE,
      browser.external = TRUE,
      browser.fill = TRUE,
      padding = 10
    )
  )
}


#' Add a dependency to Polyfill.io
#'
#' 'Shufflejs' doesn't work properly in Internet Explorer, you can use this function to make it work.
#' It load a Polyfill from \url{https://polyfill.io/v2/docs/}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Use in UI
#' fluidPage(
#'   use_polyfill()
#' )
#'
#' # Use in a chunk
#' ```{r, echo=FALSE}
#' shufflecards:::use_polyfill()
#' ```
#' }
use_polyfill <- function() {
  attachDependencies(
    tags$div(),
    polyfill_dependencies()
  )
}

