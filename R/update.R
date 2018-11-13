
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
#' if (interactive()) {
#'   library(ggplot2)
#'   library(dplyr)
#'   library(shiny)
#'   library(shufflecards)
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
#'         mutate(
#'           ind = type %in% type_num &
#'             num >= input$val_num[1] &
#'             num <= input$val_num[2] # logical indicator
#'         ) %>%
#'         select(id, ind) %>% # keep only id & indicator
#'         filter_cards(
#'           session = session,
#'           shuffleId = "gridNum",
#'           filter_by = "id",
#'           filter_list = .  # <- Use a two columns df
#'         )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
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



