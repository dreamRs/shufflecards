
#  ------------------------------------------------------------------------
#
# Title : Shuffle cards !
#    By : Victor
#  Date : 2018-11-09
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library(shufflecards)
library(shiny)




# Data --------------------------------------------------------------------

deck <- expand.grid(
  suits = c("hearts", "spades", "diamonds", "clubs"),
  values = c("a", 2:10, "j", "q", "k")
)
deck$order <- as.numeric(deck$values)




# App ---------------------------------------------------------------------


ui <- fluidPage(
  tags$h2("shufflecards example", style = "text-align: center;"),

  # Controls ----
  fluidRow(
    column(
      width = 7,
      actionButton("sortValue", "Sort by value", icon = icon("sort-numeric-asc")),
      actionButton("sortValueDesc", "Sort by value (decreasing)", icon = icon("sort-numeric-desc")),
      actionButton("sortSuit", "Sort by suits", icon = icon("sort-alpha-asc")),
      actionButton("random", "Random!", icon = icon("random"))
    ),
    column(
      width = 5,
      checkboxGroupInput(
        inputId = "suits",
        label = NULL,
        choices = c("hearts", "spades", "diamonds", "clubs"),
        selected = c("hearts", "spades", "diamonds", "clubs"),
        inline = TRUE
      )
    )
  ),

  # Shuffle ----
  shuffle_container(
    shuffleId = "grid",
    no_card = "No card to display!",
    card_list = lapply(
      X = seq_len(nrow(deck)),
      FUN = function(i) {
        shuffle_card(
          groups = deck[i, "suits"], # for filtering
          cardVal = deck[i, "order"], # for sorting
          cardSuits = deck[i, "suits"], # for sorting
          playing_card(deck[i, "suits"], deck[i, "values"])
        )
      }
    )
  )
)

server <- function(input, output, session) {

  update_shuffle("grid")

  # Sorting ----
  observeEvent(input$sortValue, {
    arrange_cards("grid", "cardVal")
  })
  observeEvent(input$sortValueDesc, {
    arrange_cards("grid", "cardVal", desc = TRUE)
  })
  observeEvent(input$sortSuit, {
    arrange_cards("grid", "cardSuits")
  })
  observeEvent(input$random, {
    randomize_cards("grid")
  })

  # Filtering ----
  observeEvent(input$suits, {
    filter_cards_groups("grid", input$suits)
  }, ignoreNULL = FALSE)

}

shinyApp(ui, server)

