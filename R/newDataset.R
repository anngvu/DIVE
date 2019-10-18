#' Shiny module UI for a dataset input
#'
#' An extension of \code{\link{dataUploadUI}} to include an additional text field.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return A \code{shiny::\link[shiny]{tagList}} containing input UI.
#' @export
newDatasetInput <- function(id) {
  ns <- NS(id)
  tags$div(id = "newDatasetInput",
    fluidRow(
      column(1),
      column(4,
             div(id = "dataUpload",
                 textInput(ns("name"), "", value = "", width = 200))
      ),
      column(7,
          dataUploadUI(ns("upload")))
      )
    )
}

#' Shiny module server function for handling new dataset input
#'
#' This is essentially the \code{\link{dataUpload}} module, but the uploaded data
#' is modified to include a key-like column before it is returned as the reactive object.
#' Use \code{\link{dataUpload}} if there is no need for a \preformatted{datakey},
#' which is used to track the dataset in the same manner as in \code{\link{refSubset}}.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param datakey Optional, a named list containing name/label for creating a key-like column.
#' @inheritParams dataUpload
#' @return A reactive data.table of the processed upload.
#' @export
newDataset <- function(input, output, session,
                      datakey = NULL, xname = NULL,
                      checkFun = NULL, informd = NULL, appdata = NULL) {

  # ------------------------------------------------------------- #

  newData <- reactiveVal(NULL)

  updateTextInput(session, "name", xname) # gives dataset a default name

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
