#' Shiny module UI for customized user-uploaded dataset
#'
#' An extension of \code{\link{dataUploadUI}}
#'
#' The UI extends \code{\link{dataUploadUI}} with an additional text field
#' for a custom data value that can be appended to the uploaded data.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Optional, a label for the name text input field.
#' @family customDataset functions
#' @export
customDatasetInput <- function(id, label = NULL) {
  ns <- NS(id)
  tags$div(id = ns("new-dataset-input"),
           textInput(ns("datavalue"), label = label, width = 200),
           dataUploadUI(ns("upload"))
  )
}

#' Shiny module server for customized user-uploaded dataset
#'
#' An extension of \code{\link{dataUploadServer}}
#'
#' The server extends \code{\link{dataUploadServer}} by appending to the uploaded data
#' a custom data column with name specified by \code{customdata} and value filled from user input.
#' This is conceptually the same as described in \code{\link{dataSubsetServer}},
#' with the only difference being that \code{customdata} is appended to a user-uploaded
#' dataset instead of an existing subsettable dataset.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param customdata Optional, a named list containing name/label to be used in creating the key column.
#' @param defaultvalue Optional, a default value to use.
#' @inheritParams dataUploadServer
#' @return A reactive \code{data.table} of the modified uploaded data.
#' @family customDataset functions
#' @export
customDatasetServer <- function(id,
                                customdata, defaultvalue = NULL,
                                checkfun = NULL, informd = NULL, appdata = NULL) {

  moduleServer(id, function(input, output, session) {

    customData <- reactiveVal(NULL)

    updateTextInput(session, "datavalue", value = defaultvalue)

    upload <- dataUploadServer("upload",
                               checkFun = checkFun,
                               informd = informd,
                               appdata = appdata)

    # ------------------------------------------------------------- #

    observeEvent(upload(), {
      dataset <- upload()
      value <- if(input$datavalue != "") input$datavalue else defaultvalue
      dataset[[customdata]] <- rep(value, nrow(dataset))
      customData(dataset)
    })

    return(customData)
  })
}
