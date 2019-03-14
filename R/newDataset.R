#' Shiny module UI for a dataset input
#'
#' An extension of \code{\link{dataUploadUI}} to include an additional text field.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Name of the dataset.
#' @param type Optional, a keyword to describe type of dataset, included in label text for additional UI customization.
#' @return A \code{shiny::\link[shiny]{tagList}} containing input UI.
#' @export
newDatasetInput <- function(id, label = id, type = "") {
  ns <- NS(id)
  tags$div(id = "newDatasetInput",
    fluidRow(
      column(1),
      column(3,
             h3(label)
      ),
      column(8,
          dataUploadUI(ns("upload")),
          fluidRow(
             column(8, div(id = "dataUpload",
                 textInput(ns("name"), "Label (optional)", value = "",
                           placeholder = "e.g. 'TEDDY', 'pilot'.."))
                 ),
             column(4, br(),
                 checkboxInput(ns("external"), paste("External", type), value = T)
             ))
      )
      ))
}

#' Shiny module server function for handling new dataset input
#'
#' This is essentially the \code{\link{dataUpload}} module, but the uploaded data
#' is modified to include a key-like column before it is returned as the reactive object.
#' Use \code{\link{dataUpload}} if there is no need for \preformatted{refkey},
#' which is used to track the dataset in the same manner as in \code{\link{refSubset}}.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refkey Optional, a named list containing name/label for creating a key-like column.
#' @inheritParams dataUpload
#' @return A reactive data.table of the processed upload.
#' @export
newDataset <- function(input, output, session,
                      refkey, infoRmd, appdata) {

  # ------------------------------------------------------------- #

  newData <- reactiveVal(NULL)

  upload <- callModule(dataUpload, "upload", infoRmd = infoRmd, appdata = appdata)

  # ------------------------------------------------------------- #
  observeEvent(upload(), {
    dataset <- upload()
    if(!is.null(refkey)) {
      name <- isolate(input$name)
      name <- if(name != "") name else refkey[[1]]
      dataset[[names(refkey)]] <- rep(name, nrow(dataset))
    }
    newData(dataset)
  })

  return(newData)
}
