#' Shiny module UI for a dataset input
#'
#' An extension of \code{\link{dataUploadUI}}
#'
#' When a user uploads a dataset, that dataset may need to be referenced either by
#' the upload number, the original file name, or even a field in the dataset file.
#' A reference is especially relevant if there are multiple datasets to be joined.
#' The most preferable way to make a reference can be to attach a meaningful label/key
#' to a dataset to be used by the application. This module simply bundles a text field
#' with the \code{\link{dataUploadUI}} module so the user can give their uploaded data
#' that meaningful label/key.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Optional, a label for the name text input field.
#' @return A \code{shiny::\link[shiny]{tagList}} containing input UI.
#' @export
newDatasetInput <- function(id, label = "(name)") {
  ns <- NS(id)
  tags$div(id = ns("newDatasetInput"),
    fluidRow(
      column(1),
      column(4,
             div(id = "dataUpload",
                 textInput(ns("name"), label, value = "", width = 200))
      ),
      column(7,
          dataUploadUI(ns("upload")))
      )
    )
}

#' Shiny module server function for handling new dataset input
#'
#' This is essentially the \code{\link{dataUpload}} module, but the uploaded data
#' is modified to include a key-like column before it is returned as a reactive object.
#' Use \code{\link{dataUpload}} if there is no need for a key,
#' which is used to track the dataset as in \code{\link{refSubset}}.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param datakey Optional, a named list containing name/label to be used in creating the key column.
#' @param xname Optional, a default name/label to use in the key column.
#' @inheritParams dataUpload
#' @return A reactive data.table of the processed upload.
#' @export
newDataset <- function(input, output, session,
                      datakey = NULL, xname = NULL,
                      checkFun = NULL, informd = NULL, appdata = NULL) {

  # ------------------------------------------------------------- #

  newData <- reactiveVal(NULL)

  updateTextInput(session, "name", value = xname) # gives dataset a default name

  upload <- callModule(dataUpload, "upload",
                       checkFun = checkFun,
                       informd = informd,
                       appdata = appdata)

  # ------------------------------------------------------------- #
  observeEvent(upload(), {
    dataset <- upload()
    if(!is.null(datakey)) {
      name <- isolate(input$name)
      name <- if(name != "") name else xname
      dataset[[datakey]] <- rep(name, nrow(dataset)) # adds a key column as specified in datakey
    }
    newData(dataset)
  })

  return(newData)
}
