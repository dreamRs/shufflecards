
#' Bootstrap thumbnail
#'
#' @param title Title of the thumbnail.
#' @param media A HTML tag img for displaying an image.
#' @param content Description to be placed in the thumbnail.
#' @param href URL to redirect when media or title is clicked.
#' @param height Height of the thumbnail.
#'
#' @export
#'
#' @importFrom htmltools tags validateCssUnit
#'
#' @examples
#' if (interactive()) {
#'
#'
#'
#' }
thumbnail <- function(title, media, content, href = NULL, height = NULL) {
  tags$div(
    class = "thumbnail",
    style = if (!is.null(height))
      paste0("height: ", validateCssUnit(height), ";"),
    tags$a(
      href = href, target = "_blank", class = "thumbnail", style = "margin-bottom: 5px",
      tags$div(
        class = "item-image", style = "text-align: center;", media
      )
    ),
    tags$h4(tags$b(tags$a(title, href = href, target = "_blank")), class = "thumbnail-title"),
    tags$div(class = "thumbnail-content", content)
  )
}


