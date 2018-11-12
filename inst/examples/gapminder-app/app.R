
#  ------------------------------------------------------------------------
#
# Title : shufflecards - gapminder data
#    By : Victor
#  Date : 2018-11-08
#
#  ------------------------------------------------------------------------




# Packages ----------------------------------------------------------------

library(gapminder)
library(ggplot2)
library(dplyr)
library(shufflecards)



# Data --------------------------------------------------------------------

gapminder %>%
  filter(country == "Italy") %>%
  ggplot() +
  aes(x = year, y = lifeExp) +
  geom_line() +
  labs(title = "Italy")





# App ---------------------------------------------------------------------

library(shiny)

ui <- fluidPage(
  tags$h1("Shufflecards with gapminder dataset"),

  fluidRow(
    column(
      width = 6,
      actionButton(inputId = "sortNumber", label = "Sort by number", icon = icon("sort-numeric-asc")),
      actionButton(inputId = "sortNumberDesc", label = "Sort by number (decreasing)", icon = icon("sort-numeric-desc")),
      actionButton(inputId = "sortLetter", label = "Sort by letter", icon = icon("sort-alpha-asc"))
    ),
    column(
      width = 6,
      checkboxGroupInput(
        inputId = "countries",
        label = NULL,
        choices = c("Italy", "France", "Germany", "Spain", "UK"),
        selected = c("Italy", "France", "Germany", "Spain", "UK"),
        inline = TRUE
      )
    )
  ),

  # verbatimTextOutput(outputId = "res"),

  shuffle_container(
    shuffleId = "grid",
    # style = "width: 1250px; margin: auto;",
    no_card = "No country match selection...",
    shuffle_card(
      groups = "Italy", id = "italy",
      number = 2, letter = "i",
      plotOutput(outputId = "italy"), width = "400px"
    ),
    shuffle_card(
      groups = "France", id = "france",
      number = 4, letter = "f",
      plotOutput(outputId = "france"), width = "400px"
    ),
    shuffle_card(
      groups = "Germany", id = "germany",
      number = 1, letter = "g",
      plotOutput(outputId = "germany"), width = "400px"
    ),
    shuffle_card(
      groups = "Spain", id = "spain",
      number = 10, letter = "s",
      plotOutput(outputId = "spain"), width = "400px"
    ),
    shuffle_card(
      groups = "UK", id = "uk",
      number = 3, letter = "u",
      plotOutput(outputId = "uk"), width = "400px"
    )
  )
)

server <- function(input, output, session) {

  # Sorting ----
  observeEvent(input$sortNumber, {
    sort_cards(session, "grid", "number", numeric = TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$sortNumberDesc, {
    sort_cards(session, "grid", "number", numeric = TRUE, decreasing = TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$sortLetter, {
    sort_cards(session, "grid", "letter")
  }, ignoreInit = TRUE)

  # Filter ----
  observeEvent(input$countries, {
    filter_cards_groups(session, "grid", input$countries)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # value ----
  output$res <- renderPrint(input$grid)

  # Elements ----
  output$italy <- renderPlot({
    gapminder %>%
      filter(country == "Italy") %>%
      ggplot() +
      aes(x = year, y = lifeExp) +
      geom_line() +
      labs(title = "Italy")
  })

  output$france <- renderPlot({
    gapminder %>%
      filter(country == "France") %>%
      ggplot() +
      aes(x = year, y = lifeExp) +
      geom_line() +
      labs(title = "France")
  })

  output$germany <- renderPlot({
    gapminder %>%
      filter(country == "Germany") %>%
      ggplot() +
      aes(x = year, y = lifeExp) +
      geom_line() +
      labs(title = "Germany")
  })

  output$spain <- renderPlot({
    gapminder %>%
      filter(country == "Spain") %>%
      ggplot() +
      aes(x = year, y = lifeExp) +
      geom_line() +
      labs(title = "Spain")
  })

  output$uk <- renderPlot({
    gapminder %>%
      filter(country == "United Kingdom") %>%
      ggplot() +
      aes(x = year, y = lifeExp) +
      geom_line() +
      labs(title = "United Kingdom")
  })

}

shinyApp(ui, server)









