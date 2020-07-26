#' Shiny module UI to display a modal with additional info
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Label, typically for specifying type of info.
#' @param i Icon type to display next to label.
#' @param link Defaults to actionLink, set false for showing actionButton
#' @return An actionLink or actionButton to call an info module.
#' @export
infoOutput <- function(id, label = "data req's", i = "exclamation-circle", link = T) {
  ns <- NS(id)
  if(link) actionLink(ns("moreInfo"), label = label, icon = icon(i)) else actionButton(ns("moreInfo"), label = label, icon = icon(i))
}

#' Server module function for displaying an info modal
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param informd Relative path to the Rmarkdown info file, whose contents will be displayed in the modal.
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
