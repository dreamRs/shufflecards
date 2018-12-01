

#' Create a Shuffle container
#'
#' @param shuffleId Shuffle's id.
#' @param ... List of \code{shuffle_card}s to include.
#' @param card_list Alternative list of \code{shuffle_card}s to include.
#' @param options Options for Shuffle, see \code{\link{shuffle_options}}.
#' @param no_card UI definition (or text) to display when all cards are filtered out.
#' @param width The width of the container, e.g. \code{'400px'}, or \code{'100\%'}; see \code{\link[htmltools]{validateCssUnit}}.
#'
#' @export
#'
#'
#' @importFrom htmltools tags tagList attachDependencies tagAppendAttributes validateCssUnit
#' @importFrom jsonlite toJSON
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shufflecards)
#'
#'   ui <- fluidPage(
#'     tags$h2("Create a responsive grid of cards"),
#'     shuffle_container(
#'       shuffleId = "grid",
#'       width = "650px",
#'       shuffle_card(
#'         tags$div("My first card", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       ),
#'       shuffle_card(
#'         tags$div("Second one", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       ),
#'       shuffle_card(
#'         tags$div("Third one", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       ),
#'       shuffle_card(
#'         tags$div("Fourth one", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
shuffle_container <- function(shuffleId, ..., card_list = NULL, options = shuffle_options(), no_card = NULL, width = NULL) {
  if (!(is.character(shuffleId) & length(shuffleId) == 1))
    stop("'shuffleId' must be a character of length one.", call. = FALSE)
  if (!inherits(options, "shuffle.options"))
    stop("'options' must be generated with 'shuffle_options'", call. = FALSE)
  args <- list(...)
  nargs <- names(args)
  if (is.null(nargs))
    nargs <- rep_len("", length(args))
  cards <- c(args[nzchar(nargs) == 0], card_list)
  cards <- validate_cards(cards, shuffleId)
  args <- args[nzchar(nargs) > 0]
  shuffleTag <- tags$div(
    id = shuffleId, class = "shuffle-container",
    style = if (!is.null(width))
      paste0("width: ", validateCssUnit(width), ";"),
    tagList(cards),
    tags$div(class = paste("col-1@sm", paste0(shuffleId, "-sizer-element"))),
    tags$script(
      type = "application/json",
      `data-for` = shuffleId,
      `data-eval` = toJSON(options$eval),
      toJSON(options$options, auto_unbox = TRUE, json_verbatim = TRUE)
    )
  )
  shuffleTag <- do.call(tagAppendAttributes, c(list(tag = shuffleTag), args))
  tagList(
    attachDependencies(shuffleTag, shuffle_dependencies()),
    tags$div(no_card, id = paste0(shuffleId, "-nodata"), style = "display: none;", class = "shuffle-nodata"),
    init_md(shuffleId)
  )
}


#' Options for Shuffle
#'
#' @param is_centered Attempt to center grid items in each row.
#' @param column_width A static number or function that returns a number which tells the plugin how wide the columns are (in pixels).
#'  If function use \code{I()} to treat as literal JavaScript.
#' @param gutter_width A static number or function that tells the plugin how wide the gutters between columns are (in pixels).
#'  If function use \code{I()} to treat as literal JavaScript.
#' @param speed Transition/animation speed (milliseconds).
#' @param easing CSS easing function to use, for example: \code{'ease'} or \code{'cubic-bezier(0.680, -0.550, 0.265, 1.550)'}.
#' @param ... Additional arguments, see \url{https://vestride.github.io/Shuffle/}
#'
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shufflecards)
#'   library(ggplot2)
#'
#'
#'   ui <- fluidPage(
#'     tags$h2("Responsive Shuffle Grid"),
#'     fluidRow(
#'       column(
#'         width = 12,
#'         shuffle_container(
#'           shuffleId = "grid",
#'           options = shuffle_options(
#'             is_centered = FALSE,
#'             column_width = I("function(containerWidth) {return 0.49 * containerWidth;}"),
#'             gutter_width = I("function(containerWidth) {return 0.01 * containerWidth;}")
#'           ),
#'           shuffle_card(
#'             plotOutput(outputId = "plot1"), width = "49%"
#'           ),
#'           shuffle_card(
#'             plotOutput(outputId = "plot2"), width = "49%"
#'           ),
#'           shuffle_card(
#'             plotOutput(outputId = "plot3"), width = "49%"
#'           )
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     output$plot1 <- renderPlot({
#'       ggplot() + geom_text(aes(1, 1, label = 1), size = 50)
#'     })
#'     output$plot2 <- renderPlot({
#'       ggplot() + geom_text(aes(1, 1, label = 2), size = 50)
#'     })
#'     output$plot3 <- renderPlot({
#'       ggplot() + geom_text(aes(1, 1, label = 3), size = 50)
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
shuffle_options <- function(is_centered = NULL, column_width = NULL, gutter_width = NULL, speed = NULL, easing = NULL, ...) {
  opts <- list(
    is_centered = is_centered,
    column_width = column_width,
    gutter_width = gutter_width,
    speed = speed,
    easing = easing
  )
  opts <- c(opts, list(...))
  names(opts) <- snake_to_camel(names(opts))
  opts <- dropNulls(opts)
  res <- list(
    options = lapply(setNames(opts, names(opts)), function(x) {
      if (inherits(x, "AsIs")) {
        x <- as.character(x)
      }
      x
    }),
    eval = get_eval(opts)
  )
  class(res) <- c(class(res), "shuffle.options")
  res
}


#' Shuffle card element
#'
#' @param ... UI elements to include within the card. Named elements canb be used to arrange cards.
#' @param groups Character vector of groups used to filtering.
#' @param id Cards's id, can be useful to filter cards server-side.
#' @param title Optional title, it will be wrapped in a H3 tag and can be updated from the server.
#' @param border Logical, add borders to the card.
#' @param closable Logical, add a button to remove the card, can't be reversed!
#' @param class CSS class(es) to apply on the card.
#' @param style Inline CSS to apply on the card.
#' @param width,height The width / height of the container, e.g. \code{'400px'}, or \code{'100\%'}; see \code{\link[htmltools]{validateCssUnit}}.
#'
#' @export
#'
#' @importFrom htmltools tag tagAppendAttributes validateCssUnit
#' @importFrom jsonlite toJSON
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shufflecards)
#'
#'   ui <- fluidPage(
#'     tags$h2("Arrange & filter a responsive grid of cards"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         radioButtons(
#'           inputId = "arrange",
#'           label = "Arrange:",
#'           choices = c("number", "letter")
#'         ),
#'         checkboxGroupInput(
#'           inputId = "filter",
#'           label = "Filter:",
#'           choices = c("red", "blue"),
#'           selected = c("red", "blue")
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         shuffle_container(
#'           shuffleId = "grid",
#'           no_card = "Nothing to display !",
#'           width = "650px",
#'           shuffle_card(
#'             num = 1, letter = "C", # for arrange
#'             groups = "red", # for filter
#'             tags$div("1 - C", style = "text-align: center; line-height: 200px"),
#'             style = "border: 3px solid red; border-radius: 5px;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           ),
#'           shuffle_card(
#'             num = 2, letter = "B", # for arrange
#'             groups = "blue", # for filter
#'             tags$div("2 - B", style = "text-align: center; line-height: 200px"),
#'             style = "border: 3px solid blue; border-radius: 5px;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           ),
#'           shuffle_card(
#'             num = 3, letter = "D", # for arrange
#'             groups = c("red", "blue"), # for filter
#'             tags$div("3 - D", style = "text-align: center; line-height: 200px"),
#'             style = "border: 3px solid; border-radius: 5px; border-color: red blue blue red;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           ),
#'           shuffle_card(
#'             num = 4, letter = "A", # for arrange
#'             groups = "red", # for filter
#'             tags$div("4 - A", style = "text-align: center; line-height: 200px"),
#'             style = "border: 3px solid red; border-radius: 5px;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           )
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     observeEvent(input$arrange, {
#'       arrange_cards(session, "grid", by = input$arrange)
#'     }, ignoreInit = TRUE)
#'
#'     observeEvent(input$filter, {
#'       filter_cards_groups(session, "grid", groups = input$filter)
#'     }, ignoreInit = TRUE, ignoreNULL = FALSE)
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
shuffle_card <- function(..., groups = NULL, id = NULL, title = NULL,
                         border = FALSE, closable = FALSE,
                         class = NULL, style = NULL, width = NULL, height = NULL) {
  args <- list(...)
  nargs <- names(args)
  has_names <- nzchar(nargs)
  if (length(has_names) > 0) {
    args <- c(
      args[-has_names],
      make_data_attr(args[has_names])
    )
  }
  if (!is.null(title)) {
    title <- list(tags$h4(title, class = "sc-title"))
  }
  if (isTRUE(closable)) {
    closable <- list(tags$span(
      class = "close hairline shufflecards-remove"
    ))
  } else {
    closable <- NULL
  }
  tag_el <- tag("div", c(closable, title, args))
  tag_attributes <- dropNulls(list(
    id = id, class = class, class = "element-item", style = style,
    class = if(isTRUE(border)) "sc-border",
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    style = if (!is.null(height)) paste0("height: ", validateCssUnit(height), ";"),
    `data-groups` = toJSON(as.character(groups)),
    style = "margin: 5px;",
    style = if(!is.null(closable) & is.null(title)) "padding-top: 30px;"
  ))
  tag_el <- do.call(tagAppendAttributes, c(list(tag = tag_el), tag_attributes))
  class(tag_el) <- c(class(tag_el), "shufflecard.tag")
  return(tag_el)
}


#' @importFrom jsonlite toJSON
#' @importFrom stats setNames
make_data_attr <- function(attrs) {
  if (any(grepl(pattern = "[^[:alnum:]]", x = names(attrs)))) {
    warning("shuffle_card: You should avoid special characters in named arguments", call. = FALSE)
  }
  attrs <- lapply(
    X = setNames(attrs, names(attrs)),
    FUN = function(x) {
      if (length(x) > 1) {
        warning("shuffle_card: Named elements in '...' must be of length one, keeping first one", call. = FALSE)
        x <- x[1]
      }
      x
    }
  )
  is_num <- vapply(attrs, is.numeric, logical(1))
  names(attrs) <- paste0("data-", names(attrs))
  attrs[["data-sc-isnum"]] <- jsonlite::toJSON(x = names(is_num)[is_num])
  attrs
}

