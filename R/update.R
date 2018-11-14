
#' Arrange cards in a Shuffle grid layout
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#' @param by Key(s) defined in \code{shuffle_card} to sort elements.
#' @param numeric Logical, set to \code{TRUE} if key is a numeric value.
#' @param desc Logical, set to \code{TRUE} to sort in decreasing order.
#'
#' @export
#'
#' @name arrange-cards
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
#'           inputId = "sort",
#'           label = "Sort by:",
#'           choices = c("Ascending order",
#'                       "Descending order",
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
#'                 value = i, # used to sort
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
#'       if (input$sort == "Ascending order") {
#'         arrange_cards(session, "gridNum", "value", numeric = TRUE)
#'       } else if (input$sort == "Descending order") {
#'         arrange_cards(session, "gridNum", "value", numeric = TRUE, desc = TRUE)
#'       } else {
#'         randomize_cards(session, "gridNum")
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
arrange_cards <- function(session, shuffleId, by, numeric = FALSE, desc = FALSE) {
  message <- list(type = "sort", sortBy = by, numeric = numeric, decreasing = desc, random = FALSE)
  session$sendInputMessage(shuffleId, message)
}

#' @export
#' @rdname arrange-cards
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
#'       filter_cards_groups(session, "gridNum", type_num)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
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
filter_cards <- function(session, shuffleId, by, filters) {
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



#' Update Shuffle Instance
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#'
#' @note Can be useful when grid layout haven't been initialized correctly.
#'
#' @export
#'
update_shuffle <- function(session, shuffleId) {
  session$sendInputMessage(shuffleId, list(type = "update"))
}



