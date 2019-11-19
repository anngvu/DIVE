#' Shiny app UI for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
matrixMixUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {
  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),
            # Control and data input module
            fluidRow(style="margin-top:30px; margin-bottom:20px;",
                     column(8, matrixCtrlUI(ns("ctrl"))),
                     column(3, dataUploadUI(ns("upload"))),
                     column(1) # to do: enable bookmarking?
            ),
            # Display module
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
#' @export
matrixMix <- function(input, output, session,
                      M, N, cdata,
                      metadata, vkey,
                      widgetmod = NULL, widgetopt = NULL,
                      factorx, dcolors,
                      informd = system.file("help/interactive_matrix.Rmd", package = "DIVE"),
                      appdata = NULL) {

  upload <- callModule(dataUpload, "upload",
                       removable = T,
                       checkFun = checkForID,
                       informd = informd,
                       appdata = appdata,
                       checkappdata = T)

  display <- callModule(matrixCtrl, "ctrl",
                        M = M, N = N, cdata = cdata, metadata = metadata, vkey = vkey,
                        newdata = upload, widgetmod = widgetmod, widgetopt = widgetopt)

  callModule(iMatrix, "matrix", display = display,
                       factorx = factorx, dcolors = dcolors)

}

#' Launch Shiny app for exploration of relationships in annotated data with an interactive matrix
#'
#' Wrapper to launch app at console
#'
#' @export
matrixMixR <- function(ns, ...) {
  ui <- matrixMixUI(ns)
  server <- function(input, output, session) {
    callModule(matrixMix, ns, ...)
  }
  shinyApp(ui, server)
}

