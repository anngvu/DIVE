#' Shiny module UI for user-uploaded data
#'
#' Create UI that contains a main \code{shiny::\link[shiny]{fileInput}}
#' and two optional features: a file-remove button and link to display more info
#' (for communicating file upload requirements and/or instructions).
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param ... Named list of params passed to \code{shiny::\link[shiny]{fileInput}}.
#' Note that \code{multiple = FALSE} (the default) must not be overridden.
#' @return UI components.
#' @family dataUpload functions
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
  tagList(dep,
          tags$div(id = ns("dataUploadUI"), class = "dataUploadUI-panel",
          tags$div(class = "ui-inline", style="margin-top:-5px;",
               tags$div(id = ns("main"),
                        tags$div(class = "ui-inline",
                                 if(length(list(...))) fileInput(ns("upload"), ...) else
                                 fileInput(ns("upload"), HTML("<strong>Upload data to compare</strong>"),
                                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                           buttonLabel = "Data", placeholder = "  no file selected", width = 275))
                        )
              ),
          tags$div(class = "ui-inline", uiOutput(ns("info")))
    )
  )
}

#' Shiny server module function for user-uploaded data
#'
#' Return reactive user-uploaded or mock data after optional checks and modifications
#'
#' At its most basic, the module returns data from \code{shiny::\link[shiny]{fileInput}}.
#' However, a function can be optionally integrated with the \code{checkFun} option
#' to perform additional light data checking or modification operations,
#' making the the module more tailored for different uses and data expectations.
#' (For more intense processing that might involve multiple steps and/or side effects as part of a pipeline,
#' one should really make a specialized module and pass the data into that intermediate module
#' instead of using \code{checkFun}.)
#'
#' File uploads can have \emph{reset} behavior by specifying the \code{removable} parameter,
#' where a remove button will appear after upload, allowing data to be cleared,
#' after which the module returns \code{NULL}.
#'
#' Finally, it is possible to perform a mock upload of a saved dataset, triggered externally by another input,
#' for demonstration purposes. The dataset is expected to be a .csv/.tsv file in
#' a directory within the app directory. For instance, the relative dataset path can be "appdata/Demo.csv".
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param asDT Logical flag to indicate whether data returned should be a \code{data.table}. If FALSE, \code{readLines} is used on file.
#' @param removable Logical flag to indicate whether data upload will have "removable" feature. Defaults to FALSE. See details.
#' @param checkFun Optional, a custom check function for an additional layer of checking/modifying uploaded data.
#' It should return a list containing message and result (result should be \code{NULL} when data fails checks).
#' @param appdata Optional, the name (including extension) of one or more files stored in appdata that can be
#' mock-uploaded. See details.
#' @param checkappdata Whether \code{checkFun} should be applied to appdata, normally FALSE.
#' @inheritParams infoServer
#' @return A \code{data.table} with a "filename" attribute containing the filename without extension,
#' or \code{NULL} depending on \code{checkFun}, or \code{NULL} if the file was cleared.
#' @family dataUpload functions
#' @export
dataUploadServer <- function(id,
                             asDT = TRUE, removable = FALSE, checkFun = NULL,
                             appdata = NULL, checkappdata = F,
                             informd = NULL) {
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
      data <- if(asDT) data.table::fread(input$upload$datapath) else readLines(input$upload$datapath)
      # perform check if checkFun is specified
      if(is.function(checkFun)) {
        checked <- checkFun(data)
        data <- checked$result
        message <- checked$message
        if(!is.null(message)) showModal(modalDialog(HTML(message), title = "Data upload status", easyClose = F))
      }
      # set new data if successful
      if(!is.null(data)) {
        attr(data, "filename") <- tools::file_path_sans_ext(input$upload$name)
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
      session$sendCustomMessage("resetFileInput", message = session$ns("upload")) # use JS to clear input
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
