


library(ggplot2)
library(shiny)
library(shufflecards)



# random letters to sort
rnd_letters <- sample(LETTERS, 12)


ui <- fluidPage(
  tags$h2("Grid of plots"),
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

}

shinyApp(ui, server)


