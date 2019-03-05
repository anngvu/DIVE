#' Shiny module UI to display a modal with additional info
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Label, typically for specifying type of info.
#' @param i Icon type to display next to label.
#' @return An actionLink.
#' @export
infoOutput <- function(id, label = "data requirements", i = "exclamation-circle") {
  ns <- NS(id)
  actionLink(ns("moreInfo"), icon = icon(i), label)
}

#' Server module function for displaying an info modal
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param infoRmd Relative path to the Rmarkdown info file, whose contents will be displayed in the modal.
#' @export
info <- function(input, output, session, infoRmd) {
  observeEvent(input$moreInfo, {
    showModal(modalDialog(
      includeMarkdown(infoRmd),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
}
