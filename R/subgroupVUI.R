#' Shiny app UI for creating comparison plots between grouped data
#'
#'
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
subgroupVUI <- function(id) {

  ns <- NS(id)
  tags$div(id = ns("viewport"), class = "subgroups-panel",
           div(align = "right", actionButton(ns("remove"), "", icon = icon("times"))),
           selectInput(ns("which1"), "Which", choices = ""),
           selectInput(ns("which2"), "Which", choices = "")
  )
}

#' Shiny module server for creating comparison plots between grouped data
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @return
#' @export
subgroupV <- function(input, output, session) {

  observeEvent(input$remove, {
    removeUI(selector = paste0("#", session$ns("viewport")))
  }, once = T)

}
