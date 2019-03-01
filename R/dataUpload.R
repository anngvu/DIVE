#' Shiny module UI for data upload
#'
#' The matrix responds to inputs, new plotdata, and has a linked drilldown component.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param
#' @param
#' @return
#' @export
dataUploadUI <- function(id, hasInfo = F) {
  ns <- NS(id)
  tags$div(id = "uploadUI",
    fluidRow(
      column(8, style="margin-top:-5px;", tags$div(id = "upload-main",
                    tags$div(id = "upload", class = "forceInline",
                             fileInput(ns("upload"), HTML("<strong>Upload data to compare</strong>"), multiple = FALSE,
                                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                       buttonLabel = "Data")))),
      column(4, br(),
             if(hasInfo) infoOutput(ns("requires"))
            )
    )
  )
}

#' Shiny server functions for dataset upload module
#'
#' It is also possible to perform a mock upload of a saved dataset, e.g. for demonstration purposes.
#' Saved datasets are expected to be .csv files that reside in a relative "Data/" directory.
#' For instance, if the name is "SampleCohort", the dataset will be "uploaded" from "Data/SampleCohort.csv".
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param infoRmd
#' @return The uploaded file as a reactive data.table object.
dataUpload <- function(input, output, session, removable = F,
                       infoRmd) {

  uploaded <- reactiveVal(NULL)

  modal <- callModule(info, "requires", infoRmd = infoRmd)

  observeEvent(input$upload, {
    data <- fread(input$upload$datapath, header = T)
    uploaded(data)
    if(removable) {
      insertUI("#upload-main", "beforeEnd",
             tags$div(id = "remove-btn", class = "forceInline",
             br(), actionButton(session$ns("remove"), "", icon = icon("times"))))
    }
  })

  observeEvent(input$remove, {
    uploaded(NULL)
    removeUI("#upload")
    removeUI("#remove-btn")
    insertUI("#upload-main", "beforeEnd",
             tags$div(id = "upload", class = "forceInline",
                      fileInput(session$ns("upload"), HTML("<strong>Upload data to compare</strong>"), multiple = FALSE,
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                buttonLabel = "Data")))
  })

  return(uploaded)
}
