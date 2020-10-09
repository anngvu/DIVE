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
                     column(9, matrixCtrlUI(ns("ctrl"))),
                     column(3, dataUploadUI(ns("upload")))
            ),
            tags$div(style = "display: flex; align-items: flex-start;",
                     dualDrilldownUI("dd"),
                     matrixMainUI(ns("matrix")),
                     matrixAsNetworkUI(ns("graph"), height = "1000px", style = "flex: 4 0 75vw; display: none;")
            )
  )
}

#' Shiny app server for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @inheritParams dataUploadServer
#' @inheritParams matrixCtrlServer
#' @inheritParams matrixMainServer
#' @inheritParams matrixAsNetworkServer
#' @inheritParams dualDrilldownServer
#' @export
matrixAppServer <- function(id,
                            M, N, P,
                            cdata, metadata, vkey,
                            factorx, colorby,
                            colorscales = list(default = list(colorscale_named(pal = "RdBu"), zmin = -1, zmax = 1)),
                            informd = system.file("info/interactive_matrix.Rmd", package = "DIVE"),
                            appdata = NULL) {

  moduleServer(id, function(input, output, session) {

    upload <- dataUploadServer("upload",
                               removable = T,
                               checkFun = DIVE::checkDataUpload,
                               informd = informd,
                               appdata = appdata,
                               checkappdata = T)

    mdata <- matrixCtrlServer("ctrl",
                              M = M, N = N, P = P,
                              cdata = cdata,
                              metadata = metadata,
                              vkey = vkey,
                              newdata = upload)

    src1 <- matrixMainServer("matrix",
                             mdata = mdata,
                             colorscales = colorscales)

    src2 <- matrixAsNetworkServer("graph",
                                  mdata, M,
                                  background = "#201037",
                                  .nodes = list(color = list(background = "lemonchiffon", border = "yellow", highlight = "white")),
                                  .edges = list(color = "yellow"),
                                  .options = list(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE, style = "display:none;")),
                                  randomSeed = 42)

    dualDrilldownServer("dd",
                        cdata = mdata$cdata,
                        colorby = colorby,
                        src1 = src1, src2 = src2)

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

