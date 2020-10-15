#' Shiny module UI for selecting a subsetted dataset
#'
#' Create a selection input that subsets a dataset
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Optional, a label for the datase;
#' currently this goes into the placeholder rather than as input label
#' @return A \code{shiny::\link[shiny]{tagList}} for UI to subset a dataset.
#' @export
#' @family dataSubset functions
dataSubsetInput <- function(id, label = NULL) {
  ns <- NS(id)
  tags$div(class = "data-subset-input", id = ns("data-subset-input"),
    tags$div(selectizeInput(ns("selectsubset"), label = NULL, choices = c(), selected = c(),
                            multiple = T, options = list(placeholder = paste("choose from", label)))),
    tags$div(uiOutput(ns("info")))
  )
}

#' Shiny server module for returning a subsetted dataset
#'
#' Create a reactive subsetted dataset based on user selection
#'
#' @details
#' This modularizes the usual means of subsetting data by selecting group(s) in a variable.
#' There can be several of these modules to create subsets from the \emph{same} dataset,
#' e.g. two modules with different grouping variables given to \code{subsetv},
#' or to create subsets from different datasets.
#' The only potentially interesting extra option is \code{customdata},
#' which appends a column to the subsetted result and exists for different purposes.
#' When there are several instances of the module for creating subsets from different datasets,
#' appending a \code{customdata} column could be useful to track the dataset origin of the subset,
#' especially if the subset data are to be merged with other data into one dataset later on.
#' For example, for two datasets that come from different sources,
#' \code{customdata} can be passed in as \code{c(Source="S1")} for the first module
#' and \code{c(Source="S2")} for the second.
#' Alternatively, \code{customdata} can be useful for appending custom data
#' such as \code{c(Color="red")}, perhaps to use in plotting of the subsetted data.
#'
#' Note: currently, multi-selection is a non-configurable default.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param dataset A \code{data.table}.
#' @param subsetv Name of the subset-by variable in the data.
#' @param subsets Optional, a list to display groups with higher-level grouping in the selection.
#' By default, the selection is composed of unique values in \code{subsetv}.
#' @param customdata Optional, a length-one character vector of data column to append. See details.
#' @inheritParams infoServer
#' @family dataSubset functions
#' @return A reactive \code{data.table} representing a subset of \code{dataset}.
#' @export
dataSubsetServer <- function(id,
                             dataset,
                             subsetv, subsets = NULL,
                             customdata = NULL,
                             informd = NULL) {

  moduleServer(id, function(input, output, session) {

    choices <- if(!is.null(subsets)) subsets else unique(dataset[[subsetv]])

    updateSelectizeInput(session, "selectsubset", choices = choices)

    # Optional info link  ------------------------------------------------------- #
    if(!is.null(informd)) {
      output$info <- renderUI({
        infoOutput(session$ns("reqs"))
      })
      modal <- infoServer("reqs", informd = informd)
    }

    # Return ------------------------------------------------------------------- #

    subsetDT <- reactive({
      shiny::validate(need(length(input$selectsubset), "Please select a subset"))
      SS <- dataset[get(subsetv) %in% input$selectsubset]
      if(!is.null(customdata)) SS[, (names(customdata)) := customdata ]
      SS
    })

    return(subsetDT)
  })

}
