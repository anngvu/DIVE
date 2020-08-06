#' Shiny module UI for data table upload module
#'
#' Generates Shiny UI for a data upload module, which contains the main file input
#' and two optional features: a file-remove button and infolink,
#' the second of which is intended to be useful for communicating file upload requirements
#' or instructions.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param ... Named list of params passed to \code{shiny::\link[shiny]{fileInput}}.
#' Note that \preformatted{multiple = FALSE} (the default) must be kept.
#' @return UI components.
#' @export
dataUploadUI <- function(id, ...) {
  ns <- NS(id)
  dep <- list(
    htmltools::htmlDependency(
      name       = "jqueryui",
      version    = "1.12.1",
      package    = "shiny",
      src        = "www/shared/jqueryui",
      script     = "jquery-ui.min.js",
      stylesheet = "jquery-ui.css"
    ),
    htmltools::htmlDependency(
      name       = "dive-app-js",
      version    = "0.1.0",
      package    = "DIVE",
      src        = "www",
      script     = "app.js"
    )
  )
  tagList(dep, tags$div(id = ns("dataUploadUI"),
      tags$div(class = "ui-inline", style="margin-top:-5px;",
               tags$div(id = ns("main"),
                        tags$div(class = "ui-inline",
                                 if(length(list(...))) fileInput(ns("upload"), ...) else
                                 fileInput(ns("upload"), HTML("<strong>Upload data to compare</strong>"),
                                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                           buttonLabel = "Data", placeholder = "  no file selected", width = 275))
                        )
              ),
      tags$div(class = "ui-inline", br(), uiOutput(ns("info")))
    )
  )
}

#' Shiny server function for data table upload module
#'
#' At its most basic, the module checks for and returns a data table from \code{fileInput}
#' (if the uploaded file is not data in table format, the return will be \preformatted{NULL}).
#'
#' A check function can be optionally integrated into this module to perform additional
#' "light" data checking or modification operations and make the the module somewhat adaptable for different uses.
#' Some example check functions simply check for specific column names or data types.
#' For more intense data processing that might involve multiple functions and/or side effects as part of a pipeline,
#' one should really make a specialized module and pass the data into that intermediate module.
#'
#' File uploads can have "reset" behavior by specifying the optional \code{removable} parameter,
#' where a remove button will appear after upload, allowing data to be "cleared", in which case the module returns \code{NULL}).
#'
#' The module also optionally incorporates \code{\link{infoOutput}} functionality to provide specifications for data.
#'
#' Finally, it is possible to perform a mock upload of a saved dataset, e.g. for demonstration purposes, that is triggered externally.
#' The dataset is expected to be a .csv/.tsv file in a relative directory within the app directory.
#' For instance, the dataset path can be "appdata/Demo.csv".
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param asDT Logical flag to indicate whether data returned should be a data.table. If FALSE, \code{readLines} is used on file.
#' @param removable Logical flag to indicate whether data upload will have "removable" feature. Defaults to FALSE. See details.
#' @param checkFun Optional, a custom check function for an additional layer of checking/modifying uploaded data.
#' It should return a list containing message and result (result should be \code{NULL} when data fails checks).
#' @param informd Optional, an Rmarkdown help file for infoOutput, e.g. requirements info.
#' @param appdata Optional, the name (including extension) of one or more files stored in appdata that can be
#' mock-uploaded. See details.
#' @param checkappdata Whether checkFun should be applied to appdata, normally FALSE.
#' @return A data.table with a "filename" attribute containing the filename without extension,
#' or \code{NULL} if the file input was not a table or returned as \code{NULL} from \code{checkFun}.
#' @export
dataUploadServer <- function(id,
                             asDT = TRUE, removable = FALSE, checkFun = NULL, informd = NULL,
                             appdata = NULL, checkappdata = F) {
  moduleServer(id, function(input, output, session) {

    uploaded <- reactiveVal(NULL)

    # Optional info link  ------------------------------------------------------- #
    if(!is.null(informd)) {
      output$info <- renderUI({
        infoOutput(session$ns("reqs"))
      })
      modal <- infoServer("reqs", informd)
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
                 tags$div(id = session$ns("remove-btn"), class = "ui-inline",
                 br(), actionButton(session$ns("remove"), "", icon = icon("trash"))))
        }
      }
    })

    observeEvent(input$remove, {
      uploaded(NULL)
      removeUI(paste0("#", session$ns("remove-btn")))
      session$sendCustomMessage("resetFileInput", message = session$ns("upload"))
    })

    # input$appdata comes from external javascript call
    observeEvent(input$appdata, {
      if(input$appdata %in% appdata) {
        data <- data.table::fread(appdata)
        if(checkappdata && is.function(checkFun)) {
          checked <- checkFun(data)
          data <- checked$result
        }
        uploaded(data)
      }
    })

    return(uploaded)
  })
}
