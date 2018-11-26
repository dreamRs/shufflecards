
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

# Sample of countries
samp_c <- c("Portugal", "Cuba", "Tunisia", "Lesotho", "Mauritania", "Chile",
            "Bangladesh", "Slovenia", "Syria", "Poland", "Djibouti", "Myanmar")



# App ---------------------------------------------------------------------

library(shiny)

ui <- fluidPage(
  tags$h1("Shufflecards with gapminder dataset"),

  fluidRow(
    column(
      width = 6,
      actionButton(inputId = "sortLifeExp", label = "Sort by life expectancy", icon = icon("sort-numeric-asc")),
      actionButton(inputId = "sortLifeExpDesc", label = "Sort by expectancy (decreasing)", icon = icon("sort-numeric-desc")),
      actionButton(inputId = "sortCountry", label = "Sort by country", icon = icon("sort-alpha-asc"))
    ),
    column(
      width = 6,
      checkboxGroupInput(
        inputId = "continent",
        label = NULL,
        choices = c("Asia", "Europe", "Africa", "Americas", "Oceania"),
        selected = c("Asia", "Europe", "Africa", "Americas", "Oceania"),
        inline = TRUE
      )
    )
  ),

  # verbatimTextOutput(outputId = "res"),

  shuffle_container(
    shuffleId = "grid",
    # style = "width: 1250px; margin: auto;",
    no_card = "No country match selection...",
    card_list = lapply(
      X = samp_c,
      FUN = function(x) {
        # Get continent for the country
        continent <- gapminder %>%
          filter(country == x) %>%
          pull(continent) %>%
          unique
        # Last life expectancy value
        lifeExp <- gapminder %>%
          filter(country == x) %>%
          pull(lifeExp) %>%
          last
        shuffle_card(
          groups = continent, # Use for filter
          country = x, # for sorting
          lifeExp = lifeExp, # for sorting
          plotOutput(outputId = x, width = "300px", height = "300px")
        )
      }
    )
  )
)

server <- function(input, output, session) {

  # Sorting ----
  observeEvent(input$sortLifeExp, {
    arrange_cards(session, "grid", "lifeExp", numeric = TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$sortLifeExpDesc, {
    arrange_cards(session, "grid", "lifeExp", numeric = TRUE, desc = TRUE)
  }, ignoreInit = TRUE)
  observeEvent(input$sortCountry, {
    arrange_cards(session, "grid", "country")
  }, ignoreInit = TRUE)

  # Filter ----
  observeEvent(input$continent, {
    filter_cards_groups(session, "grid", input$continent)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)


  # value ----
  output$res <- renderPrint(input$grid)

  # Plots ----
  lapply(
    X = samp_c,
    FUN = function(cntr) {
      output[[cntr]] <- renderPlot({
        gapminder %>%
          filter(country %in% cntr) %>%
          ggplot() +
          aes(year, lifeExp, color = country) +
          geom_point(size = 2) +
          xlim(1948, 2011) + ylim(10, 95) +
          theme_minimal() +
          scale_color_manual(values = country_colors, guide = "none") +
          labs(title = cntr, x = "Year", y = "Life expectancy (in years)")
      })
    }
  )
}

shinyApp(ui, server)









