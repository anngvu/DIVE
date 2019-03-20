#' Shiny module UI for selecting columns from a data table
#'
#' A basic UI for choosing columns in a dataset.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
selectVUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "selectVUI",
          div(class = "forceInline", uiOutput(ns("select"))),
          div(class = "forceInline", br(), actionButton(ns("go"), "select"))
          )
}

#' Shiny module server for selecting columns from a data table
#'
#' Returns the selected columns.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param data A data table.
#' @param selected Optional, initial selection.
#' @return The subsetted data table.
#' @export
selectV <- function(input, output, session,
                  data, selected = NULL) {

   output$select <- renderUI({
     selectizeInput(session$ns("var"), "Variable",
                    choices = names(data), selected = selected,
                    options = list(maxItems = 50))
   })

  V <- eventReactive(input$go, {
    data[, c("ID", input$var), with = F]
  }, ignoreInit = F)

  return(V)

}
