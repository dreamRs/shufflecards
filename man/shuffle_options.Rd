% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tags.R
\name{shuffle_options}
\alias{shuffle_options}
\title{Options for Shuffle}
\usage{
shuffle_options(
  is_centered = NULL,
  column_width = NULL,
  gutter_width = NULL,
  speed = NULL,
  easing = NULL,
  ...
)
}
\arguments{
\item{is_centered}{Attempt to center grid items in each row.}

\item{column_width}{A static number or function that returns a number which tells the plugin how wide the columns are (in pixels).
If function use \code{I()} to treat as literal JavaScript.}

\item{gutter_width}{A static number or function that tells the plugin how wide the gutters between columns are (in pixels).
If function use \code{I()} to treat as literal JavaScript.}

\item{speed}{Transition/animation speed (milliseconds).}

\item{easing}{CSS easing function to use, for example: \code{'ease'} or \code{'cubic-bezier(0.680, -0.550, 0.265, 1.550)'}.}

\item{...}{Additional arguments, see \url{https://vestride.github.io/Shuffle/}}
}
\description{
Options for Shuffle
}
\examples{
if (interactive()) {
  library(shiny)
  library(shufflecards)
  library(ggplot2)


  ui <- fluidPage(
    tags$h2("Responsive Shuffle Grid"),
    fluidRow(
      column(
        width = 12,
        shuffle_container(
          shuffleId = "grid",
          options = shuffle_options(
            is_centered = FALSE,
            column_width = I("function(containerWidth) {return 0.49 * containerWidth;}"),
            gutter_width = I("function(containerWidth) {return 0.01 * containerWidth;}")
          ),
          shuffle_card(
            plotOutput(outputId = "plot1"), width = "49\%"
          ),
          shuffle_card(
            plotOutput(outputId = "plot2"), width = "49\%"
          ),
          shuffle_card(
            plotOutput(outputId = "plot3"), width = "49\%"
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    output$plot1 <- renderPlot({
      ggplot() + geom_text(aes(1, 1, label = 1), size = 50)
    })
    output$plot2 <- renderPlot({
      ggplot() + geom_text(aes(1, 1, label = 2), size = 50)
    })
    output$plot3 <- renderPlot({
      ggplot() + geom_text(aes(1, 1, label = 3), size = 50)
    })

  }

  shinyApp(ui, server)
}
}
