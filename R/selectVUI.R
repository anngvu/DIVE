#' Shiny module UI for selecting columns from a data table
#'
#' A basic UI for choosing columns in a dataset.
#'
#' @family multiVApp module functions
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
#' @family multiVApp module functions
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param data A reactive data.table.
#' @param key A key column that is kept for every selected subset. Defaults to "ID".
#' @param label Label for variable select input.
#' @param excludepattern Exclude column names with this pattern in the selection choices.
#' @param selected Optional, initial selection.
#' @param countby The matrix representing a high-throughput dataset with sample IDs, which are intersected to generate counts in the select options. See details.
#' @return The subsetted data table.
#' @export
selectV <- function(input, output, session,
                    data = reactive({ NULL }), key = "ID", label = HTML("<strong>Phenotype/Experimental variable(s)</strong>"), excludepattern = "_(SD|SE)$",
                    selected = reactive({ NULL }), countby = reactive({ NULL }))  {

  Vdata <- reactiveVal(NULL)

  # parse a URL request for a specific dataset
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if(!is.null(query[["dataset"]])) updateSelectInput(session, "dataset", selected = query[["dataset"]])
  # })

  output$select <- renderUI({
    # shiny::req(data(), selected())
    choices <- names(data())[names(data()) != key]
    if(!is.null(excludepattern)) choices <- grep(excludepattern, choices, value = TRUE, invert = T)
    ids <- rownames(countby()[[1]])
    if(length(ids)) {
      counts <- colSums(!is.na(data()[get(key) %in% ids, choices, with = F]))
      counts <- sort(counts, decreasing = T)
      choices <- setNames(names(counts), paste0(names(counts), " (", counts, ")"))
    }
    tags$div(
      div(class = "forceInline",
          selectizeInput(session$ns("var"), label = label,
                         choices = choices, selected = selected(),
                         width = "450px", options = list(maxItems = 3))
          ),
      div(class = "forceInline",
          selectInput(session$ns("sortby"), "Sort by", choices = selected(), selected = selected(), width = "300px"))
      )
  })

  observeEvent(data(), {
    Vdata(data()[, c(key, selected()), with = F])
  })

  observeEvent(input$var, {
    if(is.null(input$var)) {
      Vdata(NULL)
    } else {
      sortby <- if(input$sortby %in% input$var) input$sortby else last(input$var)
      updateSelectizeInput(session, "sortby", choices = input$var, selected = sortby)
      data <- data()[, c(key, input$var), with = F]
      setorderv(data, cols = sortby, na.last = T)
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
