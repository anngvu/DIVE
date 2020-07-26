#' Shiny app UI for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
matrixAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {
  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),
            # Filter controls and data input
            fluidRow(style="margin-top:30px; margin-bottom:20px;",
                     column(8, matrixCtrlUI(ns("ctrl"))),
                     column(3, dataUploadUI(ns("upload"))),
                     column(1) # to do: enable bookmarking?
            ),
            fluidRow(iMatrixUI(ns("matrix")))
  )
}

#' Shiny app server for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param M A data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param N A matrix of the same dimensions as M with data for the filterable layer, e.g. sample size.
#' @param cdata The non-reactive data used for generating the matrix.
#' @param metadata A data.table with "Variable" as a key column and any number of columns (metadata) to be used as filters.
#' @param vkey
#' @param checkFun
#' @param factorx
#' @param dcolors
#' @param informd
#' @param appdata
#' @export
matrixAppServer <- function(id,
                            M, N, cdata,
                            metadata, vkey,
                            checkFun = DIVE::checkDataUpload,
                            widgetmod = NULL, widgetopt = NULL,
                            factorx, dcolors,
                            informd = system.file("help/interactive_matrix.Rmd", package = "DIVE"),
                            appdata = NULL) {

  moduleServer(id, function(input, output, session) {

    upload <- dataUploadServer("upload",
                               removable = T,
                               checkFun = checkFun,
                               informd = informd,
                               appdata = appdata,
                               checkappdata = T)

    mdata <- matrixCtrlServer("ctrl",
                              M = M, N = N,
                              cdata = cdata,
                              metadata = metadata,
                              vkey = vkey,
                              newdata = upload,
                              widgetmod = widgetmod, widgetopt = widgetopt)

    mat <- iMatrixServer("matrix",
                         mdata = mdata,
                         factorx = factorx, dcolors = dcolors)

    })
}

#' Launch Shiny app for exploration of relationships in annotated data with an interactive matrix
#'
#' Wrapper to launch app at console
#'
#' @export
matrixApp <- function(ns, ...) {
  ui <- matrixAppUI(ns)
  server <- matrixAppServer(ns, ...)
  shinyApp(ui, server)
}

