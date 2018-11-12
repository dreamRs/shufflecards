
#' Playing cards
#'
#' @param suit The name of the suit (e.g. \code{"hearts"}).
#' @param value The value (e.g. \code{"9"} for a nine).
#'
#' @export
#'
#' @importFrom htmltools tags
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shufflecards)
#'
#'   ui <- fluidPage(
#'     tags$h3("Playing cards"),
#'     fluidRow(
#'       column(
#'         width = 3,
#'         "King of Heart",
#'         playing_card("heart", "k")
#'       ),
#'       column(
#'         width = 3,
#'         "Random:",
#'         uiOutput("random"),
#'         actionButton("draw", "Draw")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     output$random <- renderUI({
#'       input$draw
#'       playing_card(
#'         sample(c("hearts", "spades", "diamonds", "clubs"), 1),
#'         sample(c("a", 2:10, "j", "q", "k"), 1)
#'       )
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
playing_card <- function(suit = c("hearts", "spades", "diamonds", "clubs"),
                         value = c("a", 2:10, "j", "q", "k")) {
  suit <- match.arg(as.character(suit), c("hearts", "spades", "diamonds", "clubs"))
  value <- match.arg(as.character(value), c("a", 2:10, "j", "q", "k"))
  tags$div(
    class = sprintf("card card-%s card-%s", suit, value),
    tags$span(),
    playing_cards_dependencies()
  )
}



