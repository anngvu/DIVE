#' Shiny module UI for controlling multi-column view
#'
#' Shiny module UI for controlling multi-column view
#'
#' Multiple high dimensional datasets can be represented by multiple \code{\link{multiVUI}} modules.
#' This module controls selection/sourcing of the available datasets.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
multiVCtrlUI <- function(id) {
  ns <- NS(id)
  tags$div(
    fluidRow(style="margin-top:30px; margin-bottom:20px; margin-right:100px",
           div(class = "forceInline", selectizeInput(ns("dataset"), "nPOD high-throughput datasets",
                                      choices = NULL, selected = NULL, multiple = T)),
           div(class = "forceInline", dataUploadUI(ns("hdata")))
    )
  )
}

#' Shiny module server for controlling multi-column view
#'
#' Implements sourcing datasets for \code{\link{multiVUI}} modules in the application page.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param hdlist A list containing high dimensional datasets;
#' the names of each element are used as choices in the selection menu.
#' @return A data matrix for the parameter \preformatted{hdata} in the \code{\link{multiV}} module.
#' @export
multiVCtrl <- function(input, output, session,
                      hdlist) {

  inview <- c()
  view <- reactiveVal(NULL)

  updateSelectizeInput(session, "dataset", choices = c(names(hdlist)), selected = NULL)

  observe({
    if(!length(input$dataset)) {
      dataset <- list(NULL)
      names(dataset) <-  paste0("i", which(names(hdlist) %in% inview))
      inview <<- c()
    } else {
      hdname <- setdiff(input$dataset, inview)
      if(length(hdname)) {
        dataset <- hdlist[hdname]
      } else {  # remove from view
        hdname <- setdiff(inview, input$dataset)
        dataset <- list(NULL)
      }
      inview <<- isolate(input$dataset)
      names(dataset) <- paste0("i", which(names(hdlist) %in% hdname)) # replace name with index # in hdlist
    }
    view(dataset)
  })

  return(view)
}
