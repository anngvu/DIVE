#' Shiny module UI to display a modal on click
#'
#' Create a clickable link or button that displays a modal dialog with customizable content
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Label, typically for specifying type of info, defaults to "Req's".
#' @param i Font-awesome icon name to display next to label, see \code{shiny::\link[shiny]{icon}}.
#' @param link Whether to use link or button. Defaults to \code{actionLink}, set FALSE to use \code{actionButton}.
#' @return An actionLink or actionButton that calls a modal.
#' @import shiny
#' @export
infoOutput <- function(id, label = "req's", i = "exclamation-circle", link = T) {
  ns <- NS(id)
  if(link) actionLink(ns("moreInfo"), label = label, icon = icon(i)) else actionButton(ns("moreInfo"), label = label, icon = icon(i))
}

#' Server module function to display a modal on click
#'
#' Display content from Rmarkdown file when link or button is clicked
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param informd Relative path to the Rmarkdown file, whose contents will be displayed in the modal.
#' @import shiny
#' @export
infoServer <- function(id, informd) {

  moduleServer(id, function(input, output, session) {
    observeEvent(input$moreInfo, {
      showModal(modalDialog(
        includeMarkdown(informd),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
}
