#' Add a card in a Shuffle grid
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#' @param card A \code{\link{shuffle_card}} to add.
#' @param where Where to add card : at the beginning or at the end. But careful
#'  it can depends on the last arrangement made by the user ! (Check example)
#'
#' @importFrom htmltools doRenderTags tagAppendAttributes tags
#' @importFrom shiny insertUI
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shufflecards)
#'   library(ggplot2)
#'
#'   ui <- fluidPage(
#'     tags$h2("Add cards to a Shuffle grid"),
#'     actionButton(inputId = "add", label = "Add a new card!"),
#'     actionButton(inputId = "arrange", label = "Arrange cards"),
#'     actionButton(inputId = "arrangedesc", label = "Arrange cards (desc)"),
#'     actionButton(inputId = "random", label = "Randomize cards"),
#'     shuffle_container(
#'       shuffleId = "grid",
#'       shuffle_card(
#'         id = "card1", number = 1,
#'         plotOutput(
#'           outputId = "plot1",
#'           width = "250px",
#'           height = "250px"
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     # First plot
#'     output$plot1 <- renderPlot({
#'       ggplot() + geom_text(aes(1, 1, label = 1), size = 50)
#'     })
#'
#'     # counter of cards
#'     counter <- reactiveVal(1)
#'     # Update counter when button is clickeck
#'     observeEvent(input$add, {
#'       newValue <- counter() + 1
#'       counter(newValue)
#'     }, ignoreInit = TRUE)
#'
#'     # When counter change add a card
#'     observeEvent(counter(), {
#'       num <- counter()
#'       add_card(
#'         session = session,
#'         shuffleId = "grid",
#'         card = shuffle_card(
#'           id = paste0("card", num), number = num,
#'           plotOutput(outputId = paste("plot", num), width = "250px", height = "250px")
#'         )
#'       )
#'       output[[paste("plot", num)]] <- renderPlot({
#'         ggplot() + geom_text(aes(1, 1, label = num), size = 50)
#'       })
#'     }, ignoreInit = TRUE)
#'
#'
#'     # Arrange ----
#'     observeEvent(input$arrange, {
#'       arrange_cards(session, "grid", "number", numeric = TRUE)
#'     })
#'     observeEvent(input$arrangedesc, {
#'       arrange_cards(session, "grid", "number", numeric = TRUE, desc = TRUE)
#'     })
#'     observeEvent(input$random, {
#'       randomize_cards(session, "grid")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
add_card <- function(session, shuffleId, card, where = c("after", "before")) {
  where <- match.arg(where)
  validate_card(card)
  addingTag <- tags$div()
  addingTag <- do.call(tagAppendAttributes, c(list(tag = addingTag), card$attribs))
  if (is.null(addingTag$attribs$id)) {
    cardId <- paste0(shuffleId, "-", sample.int(1e6, 1))
    addingTag$attribs$id <- cardId
  } else {
    cardId <- addingTag$attribs$id
  }
  message <- list(
    type = "add",
    element = doRenderTags(addingTag),
    id = cardId
  )
  # session$sendInputMessage(shuffleId, message)
  insertUI(
    session = session,
    # immediate = FALSE,
    # selector = paste0("#", cardId),
    # ui = tagList(card$children)
    immediate = TRUE,
    selector = paste0("#", shuffleId),
    ui = card,
    where = if (where == "after") "beforeEnd" else "afterBegin"
  )
  session$sendInputMessage(shuffleId, message)
}



#' Remove a card in a Shuffle grid
#'
#' @param session The \code{session} object passed to function given to shinyServer.
#' @param shuffleId The id of the shuffle container.
#' @param cardId The id of the card to remove.
#'
#' @export
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shufflecards)
#'
#'   ui <- fluidPage(
#'     tags$h2("Remove a card"),
#'     selectInput(
#'       inputId = "select",
#'       label = "Select card to remove",
#'       choices = paste0("card", 1:4)
#'     ),
#'     actionButton("remove", "Remove it!"),
#'     shuffle_container(
#'       shuffleId = "grid",
#'       shuffle_card(
#'         id = "card1",
#'         tags$div("My first card", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       ),
#'       shuffle_card(
#'         id = "card2",
#'         tags$div("Second one", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       ),
#'       shuffle_card(
#'         id = "card3",
#'         tags$div("Third one", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       ),
#'       shuffle_card(
#'         id = "card4",
#'         tags$div("Fourth one", style = "text-align: center; line-height: 200px"),
#'         style = "border: 2px solid red; border-radius: 5px;",
#'         width = "300px", # better with fixed width/height
#'         height = "200px"
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$remove, {
#'       remove_card(session, "grid", input$select)
#'     }, ignoreInit = TRUE)
#'   }
#'
#'   shinyApp(ui, server)
#' }
remove_card <- function(session, shuffleId, cardId) {
  session$sendInputMessage(shuffleId, list(
    type = "remove",
    id = jsonlite::toJSON(cardId, auto_unbox = FALSE)
  ))
}


