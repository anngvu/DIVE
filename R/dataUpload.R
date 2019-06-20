#' Shiny module UI for data table upload module
#'
#' Generates Shiny UI for a data upload module, which contains the main file input
#' and two optional features: a file-remove button and infolink,
#' the second of which is intended to be useful for communicating file upload requirements
#' or instructions.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return UI components.
#' @export
dataUploadUI <- function(id, label = "<strong>Upload data to compare</strong>", buttonlabel = "Data", placeholder = "  no file selected", width = 300) {
  ns <- NS(id)
  dataUploadConf <<- list(label = label, buttonlabel = buttonlabel, placeholder = placeholder, width = width)
  tags$div(id = ns("dataUploadUI"),
      tags$div(class = "forceInline", style="margin-top:-5px;", tags$div(id = ns("main"),
              tags$div(id = ns("upload"), class = "forceInline",
                       fileInput(ns("upload"), HTML(label), multiple = FALSE,
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                 buttonLabel = buttonlabel, placeholder = placeholder, width = width)))),
      tags$div(class = "forceInline", br(), uiOutput(ns("info")))
    )
}

#' Shiny server function for data table upload module
#'
#' At its most basic, the module checks for and returns a data table from \code{fileInput}
#' (if the uploaded file is not data in table format, the return will be \code{NULL}).
#' However, a check function can be named to perform additional "light" data checking or modification operations
#' and thus customize the module somewhat for different situations. Some example check functions
#' simply look for specific column names or types. For more intense data processing that might involve
#' multiple functions and/or side effects, one should really make a specialized module and pass the data into that.
#' File uploads can have "reset" behavior by specifying the optional \code{removable} parameter,
#' where a remove button will be rendered after upload.
#' The module also optionally incorporates \code{\link{infoOutput}} functionality.
#' Finally, it is possible to perform a mock upload of a saved dataset, e.g. for demonstration purposes.
#' Saved datasets are expected to be .csv files that reside in a relative "Data/" directory.
#' For instance, if the name is "SampleCohort", the dataset will be "uploaded" from "Data/SampleCohort.csv".
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param asDT Logical flag to indicate whether data returned should be a data.table. If FALSE, \code{readLines} is used on file.
#' @param removable Logical flag to indicate whether data upload will have "removable" feature. Defaults to FALSE. See details.
#' @param checkFun Optional, a custom check function for an additional layer of checking/modifying uploaded data.
#' It should return a list containing message and result (result should be \code{NULL} for unsuccessful data).
#' @param infoRmd Optional, an Rmarkdown help file for infoOutput, e.g. requirements.
#' @param appdata Optional, the name (including extension) of one or more files stored in appdata that can be
#' mock-uploaded. See details.
#' @return A data.table with a "filename" attribute containing the filename without extension,
#' or \code{NULL} if the file input was not a table or returned as \code{NULL} from \code{checkFun}.
#' @export
dataUpload <- function(input, output, session,
                       asDT = T, removable = F, checkFun = NULL, infoRmd = NULL, appdata = NULL) {

  uploaded <- reactiveVal(NULL)

  # Optional info link  ------------------------------------------------------- #
  if(!is.null(infoRmd)) {
    output$info <- renderUI({
      infoOutput(session$ns("reqs"))
    })
    modal <- callModule(info, "reqs", infoRmd)
  }

  # ---------------------------------------------------------------------------- #
  observeEvent(input$upload, {
    data <- if(asDT) fread(input$upload$datapath) else readLines(input$upload$datapath)
    # perform check if checkFun is specified
    if(is.function(checkFun)) {
      checked <- checkFun(data)
      data <- checked$result
      message <- checked$message
      if(!is.null(message)) showModal(modalDialog(HTML(message), title = "Data upload status", easyClose = F))
    }
    # set new data if successful
    if(!is.null(data)) {
      attr(data, "filename") <- gsub(".txt$|.csv$", "", input$upload$name)
      uploaded(data)
      # add remove button if removable
      if(removable) {
        insertUI(paste0("#", session$ns("main")), "beforeEnd",
               tags$div(id = session$ns("remove-btn"), class = "forceInline",
               br(), actionButton(session$ns("remove"), "", icon = icon("trash"))))
      }
    }
  })

  newUploadUI <- function(label = dataUploadConf$label, buttonlabel = dataUploadConf$buttonlabel,
                          placeholder = dataUploadConf$placeholder, width = dataUploadConf$width) {
    insertUI(paste0("#", session$ns("main")), "beforeEnd",
             tags$div(id = session$ns("upload"), class = "forceInline",
                      fileInput(session$ns("upload"), HTML(label), multiple = FALSE,
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                buttonLabel = buttonlabel, placeholder = placeholder, width = width)))
  }

  observeEvent(input$remove, {
    uploaded(NULL)
    removeUI(paste0("#", session$ns("upload")))
    removeUI(paste0("#", session$ns("remove-btn")))
    newUploadUI()
  })

  observeEvent(input$appdata, {
    if(input$appdata %in% appdata) {
      datafile <- data.table::fread(paste0("appdata/", input$appdata))
      uploaded(datafile)
    }
  })

  return(uploaded)
}
