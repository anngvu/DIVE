#' Shiny module UI for selecting columns from a data table
#'
#' A basic UI for choosing columns in a dataset.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
selectVUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "selectVUI",
           uiOutput(ns("select"))
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

  Vdata <- reactiveVal(data[, c(key, selected), with = F])

  output$select <- renderUI({
    tags$div(
      div(class = "forceInline",
          selectizeInput(session$ns("var"), "Variable",
                         choices = names(data)[names(data) != key], selected = selected,
                         width = "450px", options = list(maxItems = 3))
          ),
      div(class = "forceInline",
          selectInput(session$ns("sortby"), "Sort by", choices = selected, selected = selected, width = "300px"))
      )
  })

  observeEvent(input$var, {
    if(is.null(input$var)) {
      Vdata(NULL)
    } else {
      selected <- if(input$sortby %in% input$var) input$sortby else last(input$var)
      updateSelectizeInput(session, "sortby", choices = input$var, selected = selected)
      data <- data[, c(key, input$var), with = F]
      setorderv(data, cols = selected, na.last = T)
      Vdata(data)
    }
  }, ignoreNULL = F)

  observeEvent(input$sortby, {
    data <- copy(Vdata())
    setorderv(data, cols = input$sortby, na.last = T)
    Vdata(data)
  })

  return(Vdata)

}
