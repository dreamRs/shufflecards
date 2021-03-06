% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tags.R
\name{shuffle_container}
\alias{shuffle_container}
\title{Create a Shuffle container}
\usage{
shuffle_container(
  shuffleId,
  ...,
  card_list = NULL,
  options = shuffle_options(),
  no_card = NULL,
  use_bs_grid = FALSE,
  width = NULL
)
}
\arguments{
\item{shuffleId}{Shuffle's id.}

\item{...}{List of \code{shuffle_card}s to include.}

\item{card_list}{Alternative list of \code{shuffle_card}s to include.}

\item{options}{Options for Shuffle, see \code{\link{shuffle_options}}.}

\item{no_card}{UI definition (or text) to display when all cards are filtered out.}

\item{use_bs_grid}{Allow to use Bootstrap grid system, e.g. same
as in \code{\link[shiny]{column}}. If \code{TRUE}, you can use
a width between 1 and 12 in \code{\link{shuffle_card}}.}

\item{width}{The width of the container, e.g. \code{'400px'},
or \code{'100\%'}; see \code{\link[htmltools]{validateCssUnit}}.}
}
\description{
Use this function inside the UI of a \strong{Shiny} application to define a Shuffle layout.
}
\examples{
if (interactive()) {
  library(shiny)
  library(shufflecards)

  ui <- fluidPage(
    tags$h2("Create a responsive grid of cards"),
    shuffle_container(
      shuffleId = "grid",
      width = "650px",
      shuffle_card(
        tags$div("My first card", style = "text-align: center; line-height: 200px"),
        style = "border: 2px solid red; border-radius: 5px;",
        width = "300px", # better with fixed width/height
        height = "200px"
      ),
      shuffle_card(
        tags$div("Second one", style = "text-align: center; line-height: 200px"),
        style = "border: 2px solid red; border-radius: 5px;",
        width = "300px", # better with fixed width/height
        height = "200px"
      ),
      shuffle_card(
        tags$div("Third one", style = "text-align: center; line-height: 200px"),
        style = "border: 2px solid red; border-radius: 5px;",
        width = "300px", # better with fixed width/height
        height = "200px"
      ),
      shuffle_card(
        tags$div("Fourth one", style = "text-align: center; line-height: 200px"),
        style = "border: 2px solid red; border-radius: 5px;",
        width = "300px", # better with fixed width/height
        height = "200px"
      )
    )
  )

  server <- function(input, output, session) {

  }

  shinyApp(ui, server)
}
}
