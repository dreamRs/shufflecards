
#' @title Arrange cards in a Shuffle grid layout
#'
#' @description Use in a \strong{Shiny} app to sort cards.
#'
#' @param shuffleId The id of the shuffle container.
#' @param by Key(s) defined in \code{shuffle_card} to sort elements.
#' @param desc Logical, set to \code{TRUE} to sort in decreasing order.
#' @param session The \code{session} object passed to function given to shinyServer.
#'
#' @export
#'
#' @name arrange-cards
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @examples
#' if (interactive()) {
#'   library(ggplot2)
#'   library(shiny)
#'   library(shufflecards)
#'
#'   # utility fun
#'   is.even <- function(x) x %% 2 == 0
#'
#'   ui <- fluidPage(
#'     tags$h2("Arrange a Shuffle Grid By Groups"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         radioButtons(
#'           inputId = "sort",
#'           label = "Sort by:",
#'           choices = c("Ascending order (numeric)",
#'                       "Descending order (numeric)",
#'                       "Ascending order (character)",
#'                       "Descending order (character)",
#'                       "Random!")
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         shuffle_container(
#'           shuffleId = "gridNum",
#'           card_list = lapply(
#'             X = 1:12,
#'             FUN = function(i) {
#'               shuffle_card(
#'                 value_num = i,
#'                 value_char = as.character(i), # used to sort
#'                 plotOutput(
#'                   outputId = paste0("plot", i),
#'                   width = "250px",
#'                   height = "250px"
#'                 )
#'               )
#'             }
#'           )
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     # Make individual plots ----
#'     lapply(
#'       X = 1:12,
#'       FUN =  function(i) {
#'         output[[paste0("plot", i)]] <- renderPlot({
#'           ggplot() + geom_text(aes(1, 1, label = i), size = 50)
#'         })
#'       }
#'     )
#'
#'     # Sorts ----
#'     observe({
#'       if (input$sort == "Ascending order (numeric)") {
#'
#'         arrange_cards("gridNum", "value_num")
#'
#'       } else if (input$sort == "Descending order (numeric)") {
#'
#'         arrange_cards("gridNum", "value_num", desc = TRUE)
#'
#'       } else if (input$sort == "Ascending order (character)") {
#'
#'         arrange_cards("gridNum", "value_char")
#'
#'       } else if (input$sort == "Descending order (character)") {
#'
#'         arrange_cards("gridNum", "value_char", desc = TRUE)
#'
#'       } else{
#'
#'         randomize_cards("gridNum")
#'
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
arrange_cards <- function(shuffleId, by, desc = FALSE, session = shiny::getDefaultReactiveDomain()) {
  message <- list(type = "sort", sortBy = by, decreasing = desc, random = FALSE)
  session$sendInputMessage(shuffleId, message)
}

#' @export
#' @rdname arrange-cards
randomize_cards <- function(shuffleId, session = shiny::getDefaultReactiveDomain()) {
  message <- list(type = "sort", random = TRUE)
  session$sendInputMessage(shuffleId, message)
}


#' @title Filter a Shuffle grid layout by groups
#'
#' @description Use in a \strong{Shiny} app to filter cards.
#'
#' @param shuffleId The id of the shuffle container.
#' @param groups Groups (defined in \code{shuffle_card}) you want to display.
#' @param session The \code{session} object passed to function given to shinyServer.
#'
#' @export
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @examples
#' if (interactive()) {
#'   library(ggplot2)
#'   library(shiny)
#'   library(shufflecards)
#'
#'   # utility fun
#'   is.even <- function(x) x %% 2 == 0
#'
#'   ui <- fluidPage(
#'     tags$h2("Filter a Shuffle Grid By Groups"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         radioButtons(
#'           inputId = "type_num",
#'           label = "Odd or Even",
#'           choices = c("All", "Odd", "Even")
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         shuffle_container(
#'           shuffleId = "gridNum",
#'           card_list = lapply(
#'             X = 1:12,
#'             FUN = function(i) {
#'               shuffle_card(
#'                 groups = ifelse(is.even(i), "even", "odd"),
#'                 plotOutput(
#'                   outputId = paste0("plot", i),
#'                   width = "250px",
#'                   height = "250px"
#'                 )
#'               )
#'             }
#'           )
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     # Make individual plots ----
#'     lapply(
#'       X = 1:12,
#'       FUN =  function(i) {
#'         output[[paste0("plot", i)]] <- renderPlot({
#'           ggplot() + geom_text(aes(1, 1, label = i), size = 50)
#'         })
#'       }
#'     )
#'
#'     # Filters ----
#'     observe({
#'       if (input$type_num == "All") {
#'         type_num <- c("even", "odd")
#'       } else {
#'         type_num <- tolower(input$type_num)
#'       }
#'
#'       # Pass a vector of groups to display
#'       filter_cards_groups("gridNum", type_num)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
filter_cards_groups <- function(shuffleId, groups, session = shiny::getDefaultReactiveDomain()) {
  if (length(groups) == 0)
    groups <- as.character(sample.int(1e6, 1))
  message <- list(type = "filter-groups", groups = groups)
  session$sendInputMessage(shuffleId, message)
}



#' @title Filter a Shuffle grid layout with custom values
#'
#' @description Use in a \strong{Shiny} app to filter cards by a custom value.
#'
#' @param shuffleId The id of the shuffle container.
#' @param by Key defined in \code{shuffle_card} to filter elements, can be \code{"id"}
#'  to refer to the unique ID associated with the card.
#' @param filters Possible values are:
#'
#'   - \strong{named list:} where names match the key used in \code{by} and values are \code{TRUE} or \code{FALSE}
#'
#'   - \strong{two-columns data.frame:} where first column contains names and second \code{TRUE} or \code{FALSE}
#'
#'   - \strong{one-column data.frame:} where the column contains cards key to display (those absent will be hided)
#'
#'   - \strong{character vector:} containing cards key to display
#' @param session The \code{session} object passed to function given to shinyServer.
#'
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' if (interactive()) {
#'   library(ggplot2)
#'   library(dplyr)
#'   library(shiny)
#'   library(shufflecards)
#'
#'
#'
#'   # utility fun
#'   is.even <- function(x) x %% 2 == 0
#'
#'   ui <- fluidPage(
#'     tags$h2("Filter a Shuffle Grid"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         radioButtons(
#'           inputId = "type_num",
#'           label = "Odd or Even",
#'           choices = c("All", "Odd", "Even")
#'         ),
#'         sliderInput(
#'           inputId = "val_num",
#'           label = "Value:",
#'           min = 1, max = 12,
#'           value = c(1, 12)
#'         )
#'       ),
#'       column(
#'         width = 9,
#'         shuffle_container(
#'           shuffleId = "gridNum",
#'           card_list = lapply(
#'             X = 1:12,
#'             FUN = function(i) {
#'               shuffle_card(
#'                 id = paste0("card", i), # set an ID to cards to use server-side
#'                 plotOutput(outputId = paste0("plot", i), width = "250px", height = "250px")
#'               )
#'             }
#'           )
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     # Make individual plots ----
#'     lapply(
#'       X = 1:12,
#'       FUN =  function(i) {
#'         output[[paste0("plot", i)]] <- renderPlot({
#'           ggplot() + geom_text(aes(1, 1, label = i), size = 50)
#'         })
#'       }
#'     )
#'
#'     # Filters ----
#'     observe({
#'       if (input$type_num == "All") {
#'         type_num <- c("even", "odd")
#'       } else {
#'         type_num <- tolower(input$type_num)
#'       }
#'
#'       # Create a df to filters values
#'       data_frame(num = 1:12) %>%
#'         mutate(
#'           id = paste0("card", num), # card's ID
#'           type = if_else(is.even(num), "even", "odd")
#'         ) %>%
#'         filter(
#'           type %in% type_num,      # filter df to keep desired cards
#'           num >= input$val_num[1],
#'           num <= input$val_num[2]
#'         ) %>%
#'         pull(id) %>% # extract only id
#'         filter_cards(
#'           session = session,
#'           shuffleId = "gridNum",
#'           by = "id",
#'           filters = .  # <- Vector of IDs to display
#'         )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
filter_cards <- function(shuffleId, by, filters, session = shiny::getDefaultReactiveDomain()) {
  stopifnot(is.character(by) & length(by) == 1)
  if (is.data.frame(filters)) {
    if (ncol(filters) > 1) {
      filters <- setNames(as.list(filters[[2]]), filters[[1]])
    } else {
      filters <- setNames(as.list(rep_len(TRUE, length(filters[[1]]))), filters[[1]])
    }
  } else if (is.atomic(filters)) {
    filters <- as.character(filters)
    filters <- setNames(as.list(rep_len(TRUE, length(filters))), filters)
  } else {
    filters <- as.list(filters)
    if (!all(nzchar(names(filters)))) {
      stop("'filters' must be named !", call. = FALSE)
    }
  }
  filters <- lapply(
    X = setNames(filters, names(filters)),
    FUN = function(x) {
      if (length(x) > 1) {
        warning("'filters' elements must be of length one, keeping first one", call. = FALSE)
        x <- x[1]
      }
      if (is.na(x)) {
        warning("'filters' doesn't support missing values, treating as FALSE", call. = FALSE)
        x <- FALSE
      }
      if (!is.logical(x)) {
        stop("'filters' elements must be TRUE or FALSE", call. = FALSE)
      }
      x
    }
  )
  if (by != "id") {
    by <- paste("data", by, sep = "-")
  }
  message <- list(type = "filter-custom", filterBy = by, filterList = filters)
  session$sendInputMessage(shuffleId, message)
}



#' @title Update Shuffle Instance
#'
#' @description Use in a \strong{Shiny} app to update cards layout.
#'
#' @param shuffleId The id of the shuffle container.
#' @param session The \code{session} object passed to function given to shinyServer.
#'
#' @note Can be useful when grid layout haven't been initialized correctly.
#'
#' @export
#'
update_shuffle <- function(shuffleId, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(shuffleId, list(type = "update"))
}




#' @title Update meta-data of a card
#'
#' @description Use in a \strong{Shiny} app to update data associated to a card.
#'
#'
#' @param shuffleId The id of the shuffle container.
#' @param cardId The id of the card to update
#' @param ... Attributes to update or set.
#' @param title New title for the card.
#' @param session The \code{session} object passed to function given to shinyServer.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shufflecards)
#'
#'   ui <- fluidPage(
#'     tags$h2("Update cards's meta-data"),
#'
#'     fluidRow(
#'       column(
#'         width = 3,
#'         sliderInput("val1", "Value card 1", 1, 10, 1),
#'         sliderInput("val2", "Value card 2", 1, 10, 2),
#'         sliderInput("val3", "Value card 3", 1, 10, 3),
#'         sliderInput("val4", "Value card 4", 1, 10, 4),
#'         actionButton("arrange", "Arrange cards with new values")
#'       ),
#'       column(
#'         width = 9,
#'         shuffle_container(
#'           shuffleId = "grid",
#'           shuffle_card(
#'             id = "card1",
#'             myvalue = 1,
#'             tags$div("My first card", style = "text-align: center; line-height: 200px"),
#'             style = "border: 2px solid red; border-radius: 5px;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           ),
#'           shuffle_card(
#'             id = "card2",
#'             myvalue = 2,
#'             tags$div("Second one", style = "text-align: center; line-height: 200px"),
#'             style = "border: 2px solid red; border-radius: 5px;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           ),
#'           shuffle_card(
#'             id = "card3",
#'             myvalue = 3,
#'             tags$div("Third one", style = "text-align: center; line-height: 200px"),
#'             style = "border: 2px solid red; border-radius: 5px;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           ),
#'           shuffle_card(
#'             id = "card4",
#'             myvalue = 4,
#'             tags$div("Fourth one", style = "text-align: center; line-height: 200px"),
#'             style = "border: 2px solid red; border-radius: 5px;",
#'             width = "300px", # better with fixed width/height
#'             height = "200px"
#'           )
#'         )
#'       )
#'     )
#'
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     # Update cards ----
#'     observeEvent(input$val1, {
#'       update_card("grid", "card1", myvalue = input$val1)
#'     }, ignoreInit = TRUE)
#'     observeEvent(input$val2, {
#'       update_card("grid", "card2", myvalue = input$val2)
#'     }, ignoreInit = TRUE)
#'     observeEvent(input$val3, {
#'       update_card("grid", "card3", myvalue = input$val3)
#'     }, ignoreInit = TRUE)
#'     observeEvent(input$val4, {
#'       update_card("grid", "card4", myvalue = input$val4)
#'     }, ignoreInit = TRUE)
#'
#'     # Arrange ----
#'     observeEvent(input$arrange, {
#'       arrange_cards("grid", "myvalue", numeric = TRUE)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
update_card <- function(shuffleId, cardId, ..., title = NULL, session = shiny::getDefaultReactiveDomain()) {
  args <- list(...)
  if (length(args) > 0) {
    nargs <- names(args)
    if (is.null(nargs))
      stop("All arguments in '...' must be named.", call. = FALSE)
    has_names <- nzchar(nargs)
    if (!all(has_names))
      stop("All arguments in '...' must be named.", call. = FALSE)
    names(args) <- paste0("data-", names(args))
  } else {
    args <- NULL
  }
  message <- list(type = "update-card", id = cardId, args = args, title = title)
  session$sendInputMessage(shuffleId, dropNulls(message))
}






