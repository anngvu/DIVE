#' Shiny module UI for a dataset input
#'
#' A differentiated version of dataUploadUI with additional fields.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param label Name of the dataset.
#' @param type Optional, a keyword to describe type of dataset, included in label text for additional UI customization.
#' @return A \code{shiny::\link[shiny]{tagList}} containing inputs for cohort data.
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

#' Shiny server functions for dataset upload module
#'
#' It is also possible to perform a mock upload of a saved dataset, e.g. for demonstration purposes.
#' Saved datasets are expected to be .csv files that reside in a relative "Data/" directory.
#' For instance, if the name is "SampleCohort", the dataset will be "uploaded" from "Data/SampleCohort.csv".
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refkey Optional, a named list containing name/label for creating a key-like column.
#' @param appdata Optional, character name that can be used to accession a saved dataset. See details.
#' @param infoRmd Optional, relative path to an info Rmarkdown file for this module.
#' @return A reactive data.table of the processed upload.
#' @export
newDataset <- function(input, output, session,
                      refkey, infoRmd, appdata) {

  # ------------------------------------------------------------- #

  newData <- reactiveVal(NULL)

  upload <- callModule(dataUpload, "upload", infoRmd = infoRmd, appdata = appdata)

  # ------------------------------------------------------------- #
  processNew <- function(dataset) {
    names(dataset) <- make.names(names(dataset))
    if(!is.null(refkey)) {
      name <- isolate(input$name)
      name <- if(name != "") name else refkey[[1]]
      dataset[[names(refkey)]] <- rep(name, nrow(dataset))
    }
    newData(dataset)
  }

  observeEvent(upload(), {
    dataset <- upload()
    # If data looks good...
    processNew(dataset)
  })

  return(newData)
}
