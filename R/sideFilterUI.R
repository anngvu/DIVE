#' Create a switchable filter input component
#'
#' Create an input component intended to function as a filter that can be turned on/off
#'
#' This is a wrapper for creating certain Shiny input components to use as dataset filters,
#' e.g. each column of a table gets its own specified input component,
#' which is of a type enumerated in the parameter \code{type}.
#' Generally, \code{sliderInput} (range) is appropriate for numeric columns representing continuous data,
#' while \code{selectInput}, \code{selectizeInput} and \code{checkboxInput} can be used for discrete data,
#' and \code{checkboxGroupInput} can be used for list columns (especially if using \code{data.table}).
#'
#' Whether to initialize the input with none or all of the available values selected will
#' depend on the data. For a very big table, it makes the most sense to initialize inputs with
#' a selection of values that show the most relevant filtered subset rather than everything.
#'
#' The inputs have namespaced IDs "<namespace>-filter-<colname>" by applying the passed-in
#' namespacing function \code{ns} to \code{id}. To get values in a server function, use \code{\link{filter4j}}.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param values Choices for the input.
#' @param type One of \code{c("select", "selectize", "checkbox", "checkboxGroup", "range")}, corresponding to
#' \code{selectInput}, \code{selectizeInput}, \code{checkboxInput}, \code{checkboxGroupInput}, or \code{sliderInput} (range).
#' @param selected The initial selection of the input, which defaults to "first" for the first of \code{values}, but
#' can also be specified as "last", "all" or "none".
#' @param conditional If given a boolean value, will wrap element in a conditional panel with inital display given by the value,
#' with a switch element to control conditional display. Use \code{NULL} to render component without conditional control.
#' @param ns Function for namespacing components, i.e. from `session$ns`.
#' @param width Width of input elements.
#' @export
sideFilterUI <- function(inputId,
                         values,
                         type = c("select", "selectize", "checkbox", "checkboxGroup", "range"),
                         selected = NULL,
                         conditional = NULL,
                         ns = NS(NULL),
                         width = 200) {

  id <- ns(paste0("filter-", inputId))

  if(type == "range") {
    ui <- sliderInput(id, NULL, min = min(values, na.rm = T), max = max(values, na.rm = T),
                      value = c(min, max), width = width)
  } else {
    # select, selectize, checkbox, checkboxGroup
    choices <- unique(unlist(values))
    ui <- do.call(paste0(type, "Input"), list(inputId = id, label = NULL, choices = choices, selected = selected, width = width))
  }
  if(!is.null(conditional)) {
    actid <- paste("usefilter", gsub(".", "", inputId, fixed = T), sep = "_")
    ui <- div(shinyWidgets::prettySwitch(ns(actid), NULL, value = conditional, slim = TRUE, inline = TRUE),
              tags$span(inputId, class = "filter-label"), br(),
              conditionalPanel(paste0("input.",actid), ui, ns = ns, class = "ui-inline"))
  } else {
    ui <- div(h5(inputId), ui)
  }
  ui
}

