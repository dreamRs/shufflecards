


library(ggplot2)
library(shiny)
library(shufflecards)
library(dplyr)

# utility fun
is.even <- function(x) x %% 2 == 0

# random letters to sort
rnd_letters <- sample(LETTERS, 12)


ui <- fluidPage(
  tags$h2("Shuffle with Bootstrap grid"),
  fluidRow(
    column(
      width = 3,
      radioButtons(
        inputId = "sort",
        label = "Sort by:",
        choices = c(
          "Ascending order (numeric)",
          "Descending order (numeric)",
          "Ascending order (character)",
          "Descending order (character)",
          "Random!"
        )
      ),
      radioButtons(
        inputId = "type_num",
        label = "Odd or Even",
        choices = c("All", "Odd", "Even")
      ),
      sliderInput(
        inputId = "val_num",
        label = "Value:",
        min = 1, max = 12,
        value = c(1, 12)
      )
    ),
    column(
      width = 9,
      shuffle_container(
        shuffleId = "bsgrid",
        card_list = lapply(
          X = 1:12,
          FUN = function(i) {
            shuffle_card(
              id = paste0("card", i),
              value_num = i,
              value_char = rnd_letters[i], # used to sort
              plotOutput(
                outputId = paste0("plot", i),
                height = "250px",
                width = "250px"
              )
            )
          }
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Make individual plots ----
  lapply(
    X = 1:12,
    FUN =  function(i) {
      output[[paste0("plot", i)]] <- renderPlot({
        ggplot() +
          geom_text(aes(1, 1, label = paste(i, rnd_letters[i], sep = "|")), size = 30)
      })
    }
  )


  # Sorts ----
  observe({
    if (input$sort == "Ascending order (numeric)") {

      arrange_cards("bsgrid", "value_num")

    } else if (input$sort == "Descending order (numeric)") {

      arrange_cards("bsgrid", "value_num", desc = TRUE)

    } else if (input$sort == "Ascending order (character)") {

      arrange_cards("bsgrid", "value_char")

    } else if (input$sort == "Descending order (character)") {

      arrange_cards("bsgrid", "value_char", desc = TRUE)

    } else{

      randomize_cards("bsgrid")

    }
  })


  # Filters ----
  observe({
    if (input$type_num == "All") {
      type_num <- c("even", "odd")
    } else {
      type_num <- tolower(input$type_num)
    }

    # Create a df to filters values
    ids <- tibble(
      num = 1:12,
      id = paste0("card", num),
      type = if_else(is.even(num), "even", "odd")
    ) %>%
      filter(
        type %in% type_num,      # filter df to keep desired cards
        num >= input$val_num[1],
        num <= input$val_num[2]
      ) %>%
      pull(id) # extract only id
    filter_cards(
      shuffleId = "bsgrid",
      by = "id",
      filters = ids  # <- Vector of IDs to display
    )
  })


}

shinyApp(ui, server)


