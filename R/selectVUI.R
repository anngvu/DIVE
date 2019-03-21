#' Shiny module UI for selecting columns from a data table
#'
#' A basic UI for choosing columns in a dataset.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
selectVUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "selectVUI",
           div(class = "forceInline", uiOutput(ns("select")))
  )
}

#' Shiny module server for selecting columns from a data table
#'
#' Returns the selected columns.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param data A data table.
#' @param key A key column that is kept for every selected subset. Defaults to "ID".
#' @param selected Optional, initial selection.
#' @return The subsetted data table.
#' @export
selectV <- function(input, output, session,
                    data, key = "ID", selected = NULL) {

  output$select <- renderUI({
    selectizeInput(session$ns("var"), "Variable",
                   choices = names(data)[names(data) != key], selected = selected,
                   options = list(maxItems = 3))
  })

  V <- eventReactive(input$var, {
    if(is.null(input$var)) return(NULL)
    data[, c(key, input$var), with = F]
  }, ignoreNULL = F)

  return(V)

}
