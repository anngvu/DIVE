#' Shiny module UI for data upload module
#'
#' Generates Shiny UI for a data upload module, which contains the main file input
#' and two optional features: a file-remove button and infolink,
#' the second of which is intended to be useful for communicating file upload requirements
#' or instructions.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return UI components.
#' @export
dataUploadUI <- function(id) {
  ns <- NS(id)
  tags$div(id = ns("dataUploadUI"),
    fluidRow(
      column(8, style="margin-top:-5px;", tags$div(id = ns("main"),
                    tags$div(id = ns("upload"), class = "forceInline",
                             fileInput(ns("upload"), HTML("<strong>Upload data to compare</strong>"), multiple = FALSE,
                                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                       buttonLabel = "Data")))),
      column(4, br(), uiOutput(ns("info")))
    )
  )
}

#' Shiny server function for data upload module
#'
#' The most basic implementation is to process and return any input to fileInput in the UI.
#' File uploads can have "reset" behavior if the removable optional feature is specified,
#' where a remove button will be rendered after upload.
#' The module also optionally incorporates \code{\link{infoOutput}} functionality.
#' Finally, it is possible to perform a mock upload of a saved dataset, e.g. for demonstration purposes.
#' Saved datasets are expected to be .csv files that reside in a relative "Data/" directory.
#' For instance, if the name is "SampleCohort", the dataset will be "uploaded" from "Data/SampleCohort.csv".
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param removable Logical flag to indicate whether data upload will have "removable" feature. Defaults to FALSE. See details.
#' @param infoRmd Optional, an Rmarkdown help file for infoOutput, e.g. requirements.
#' @param appdata Optional, the name (including extension) of one or more files stored in appdata that can be
#' mock-uploaded. See details.
#' @return The uploaded file as a reactive data.table object.
dataUpload <- function(input, output, session,
                       removable = F, infoRmd = NULL, appdata = NULL) {

  uploaded <- reactiveVal(NULL)

  # Optional info link  ------------------------------------------------------- #
  if(!is.null(infoRmd)) {
    output$info <- renderUI({
      infoOutput(session$ns("reqs"))
    })
    modal <- callModule(info, "reqs", infoRmd = infoRmd)
  }

  # ---------------------------------------------------------------------------- #
  observeEvent(input$upload, {
    data <- fread(input$upload$datapath, header = T)
    uploaded(data)
    if(removable) {
      insertUI(paste0("#", session$ns("main")), "beforeEnd",
             tags$div(id = session$ns("remove-btn"), class = "forceInline",
             br(), actionButton(session$ns("remove"), "", icon = icon("times"))))
    }
  })

  observeEvent(input$remove, {
    uploaded(NULL)
    removeUI(paste0("#", session$ns("upload")))
    removeUI(paste0("#", session$ns("remove-btn")))
    insertUI(paste0("#", session$ns("main")), "beforeEnd",
             tags$div(id = session$ns("upload"), class = "forceInline",
                      fileInput(session$ns("upload"), HTML("<strong>Upload data to compare</strong>"), multiple = FALSE,
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                buttonLabel = "Data")))
  })

  observeEvent(input$appdata, {
    if(input$appdata %in% appdata) {
      datafile <- data.table::fread(paste0("appdata/", input$appdata))
      uploaded(datafile)
    }
  })

  return(uploaded)
}
