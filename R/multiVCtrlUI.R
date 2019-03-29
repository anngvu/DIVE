#' Shiny module UI for controlling multi-column view
#'
#' Shiny module UI for controlling multi-column view
#'
#' Multiple high dimensional datasets can be displayed by calling separate instances of \code{\link{multiVUI}} modules,
#' with the contents typically laid out in separate a row containers. This "controller" UI establishes three different ways to
#' select and source the datasets:
#' \enumerate{
#'   \item A selection menu that contains datasets that are pre-processed and stored in memory.
#'   \item An upload method for users to upload their own datasets.
#'   \item A beta (and least-supported) method for retrieving datasets from GEO.
#' }
#' For convenience and customization, it is possible make unavailable any of the three sourcing methods,
#' e.g. hide the GEO method by not displaying the UI.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param menu Logical flag, should there be a menu for sourcing stored datasets? See details.
#' @param upload Logical flag, should users be able to upload data? See details.
#' @param GEO Logical flag, should users be able to pull data from GEO? (beta) See details.
#' @export
multiVCtrlUI <- function(id, menu = T, upload = T, GEO = T) {
  ns <- NS(id)
  tags$div(id = "multiVCtrlUI", style="margin-top:30px; margin-bottom:20px; margin-right:100px",
           if(menu) div(class = "forceInline", style = "margin-right: 30px;",
               selectizeInput(ns("dataset"), HTML("<strong>Available high-throughput datasets</strong>"),
                              choices = NULL, selected = NULL, multiple = T,
                              options = list(placeholder = "select one or more datasets to view"))),
           if(upload) div(class = "forceInline", style = "margin-right: 30px;",
               dataUploadUI(ns("upload"), label = "<strong>Upload your own data</strong>")),
           if(GEO) div(class = "forceInline", style = "margin-right: 30px;",
               br(), actionButton(ns("getGEO"), "Source GEO (beta)"))
          )
}

#' Shiny module server for controlling multi-column view
#'
#' Implements sourcing datasets for \code{\link{multiVUI}} modules in the application page.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param hdlist A list containing high dimensional datasets; must have names to be used as choices in the selection menu.
#' @param choices Names referencing datasets in hdlist, to be used in selection menu.
#' When the datasets should be displayed as grouped, the choices can be passed in accordingly for \code{\link[shiny]{selectizeInput}}.
#' @param infoRmd Optional link to an Rmarkdown document containing details for the data upload module.
#' @return A list containing the data matrix for the parameter \preformatted{hdata} in the \code{\link{multiV}} module.
#' @export
multiVCtrl <- function(input, output, session,
                      hdlist, choices = names(hdlist), infoRmd = NULL) {

  inview <- c()
  view <- reactiveVal(NULL)

  updateSelectizeInput(session, "dataset", choices = choices, selected = NULL)

  observe({
    if(!length(input$dataset)) {
      dataset <- list(NULL) # set return to NULL
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

  udata <- callModule(dataUpload, "upload", infoRmd = infoRmd)

  observeEvent(udata(), {
    # process data

  })

  observe

  return(view)
}
