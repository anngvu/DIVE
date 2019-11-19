#' Shiny module UI for selecting a subsetted dataset
#'
#' Shiny module UI to subset a dataset through a selection menu.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param name Optional, name for the main dataset.
#' @param label Optional, select input label.
#' @param subsets A named list of the available subset factors in the data.
#' @return A \code{shiny::\link[shiny]{tagList}} for UI to subset a dataset.
#' @export
refSubsetInput <- function(id, name = id, label = "", subsets = "") {
  ns <- NS(id)
  tags$div(id = "refSubsetInput",
    fluidRow(
      column(1),
      column(3, span(class = "large-name", name)),
      column(4, div(id = "refdata", uiOutput(ns("selectSubsetUI")))),
      column(4, #
             br(),
             uiOutput(ns("info"))
      ))
  )
}

#' Shiny server module function for returning a subsetted dataset
#'
#' Server function for creating a reactive subsetted dataset following user interaction.
#'
#' When many instances of the module are used to create subsets from different datasets
#' (as opposed to different subsets from the same dataset), it might be necessary to use refkey
#' to track the dataset origin of the subset, and especially if the subsets are to be merged
#' on some common column. Generically speaking, datasets can come from different sources/locations,
#' so refkey can be called "Source" and the values might then be S1 and S2.
#'
#' On the other hand, when this is used to create subsets from the same dataset,
#' an "exclude list" can be passed in to make sure that the subset doesn't contain
#' certain indices/IDs. This is desirable for creating subsets that don't overlap
#' with another or for filtering out rows that don't pass some criteria.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refdata The reference data.table.
#' @param subsetfeat The name of the column containing the subset factors.
#' @param subsets Optional, a custom selection list for subsets; by default, the list is composed of unique options found in \code{subsetfeat}.
#' @param refkey Optional, a named list containing name/label for creating a key-like column,
#' where the name is the name of the column. See details for intended purpose.
#' @param exclude Optional, a list of IDs to exclude.
#' @param informd Optional, relative path to an info Rmarkdown file that can be pulled up in a modal.
#' @return A reactive subsetted data.table.
#' @export
refSubset <- function(input, output, session,
                      refdata, subsetfeat, subsets = NULL, datakey = NULL, refname = NULL, exclude = reactive({}),
                      informd = NULL) {

  # Optional info link  ------------------------------------------------------- #
  if(!is.null(informd)) {
    output$info <- renderUI({
      infoOutput(session$ns("reqs"))
    })
    modal <- callModule(info, "reqs", informd = informd)
  }

  # ---------------------------------------------------------------------------- #

  output$selectSubsetUI <- renderUI({
    choices <- if(length(subsets)) subsets else unique(refdata[[subsetfeat]])
    selectizeInput(session$ns("selectSubset"), label = "",
                     choices = subsets, selected = NULL,
                     multiple = T,
                     options = list(placeholder = "(one or more types)"))
  })

  subsetDT <- reactive({
    validate(need(length(input$selectSubset), "Please select a type subset"))
    SS <- refdata[get(subsetfeat) %in% input$selectSubset]
    if(length(exclude())) SS <- SS[!ID %in% exclude() ] # Check-later: ID is hard-coded
    if(!is.null(datakey)) SS[, (datakey) := refname ]
    SS
  })

  return(subsetDT)
}
