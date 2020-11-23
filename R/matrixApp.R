#' Shiny app UI for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @import shiny
#' @import magrittr
#' @export
matrixAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {
  ns <- NS(id)
  ns.graph <- paste(ns("graph"), "asNetwork", sep = "-")
  ns.matrix <- paste(ns("matrix"), "matrixMain", sep = "-")
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),
            # Filter controls and data input
            fluidRow(class = "top-panel matrixAppUI-panel",
                     column(12,tags$div(class = "input-panel", matrixCtrlUI(ns("ctrl"))),
                            tags$div(class = "input-panel", dataUploadUI(ns("upload"))))
            ),
            tags$div(id = ns("view-switch"), class = "btn-group", style = "margin-bottom: 20px;",
                     actionButton("viewgraph", "Graph view") %>%
                       tagAppendAttributes(class = "btn-sm", onclick = sprintf("{ $('#%s').hide();
                                                 $('#%s').trigger('hidden');
                                                 $('#%s').show();
                                                 $('#%s').trigger('shown');
                                                 $(this).addClass('active').siblings().removeClass('active'); }", ns.matrix, ns.matrix, ns.graph, ns.graph)),
                     actionButton("viewmatrix", "Matrix view") %>%
                       tagAppendAttributes(class = "btn-sm active", onclick = sprintf("{ $('#%s').hide();
                                                 $('#%s').trigger('hidden');
                                                 $('#%s').show();
                                                 $('#%s').trigger('shown');
                                                 $(this).addClass('active').siblings().removeClass('active'); }", ns.graph, ns.graph, ns.matrix, ns.matrix))
            ),
            tags$div(style = "position:relative; display: flex; align-items: flex-start;",
                     matrixMainUI(ns("matrix"), style = "flex: 4 0 70vw;"),
                     matrixAsNetworkUI(ns("graph"), height = "1000px", style = "flex: 4 0 70vw; display: none;"),
                     dualDrilldownUI(ns("dd"))
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
#' @import shiny
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
                               checkFun = checkDataUpload,
                               informd = informd,
                               appdata = appdata,
                               checkappdata = T)

    mfilter <- reactiveValues(N = NULL, P = NULL)

    mdata <- matrixCtrlServer("ctrl",
                              M = M, N = N, P = P,
                              cdata = cdata,
                              metadata = metadata,
                              vkey = vkey,
                              newdata = upload,
                              mfilter = mfilter)

    src1 <- matrixMainServer("matrix",
                             mdata = mdata,
                             colorscales = colorscales)

    src2 <- matrixAsNetworkServer("graph",
                                  mdata,
                                  mfilter,
                                  background = "#201037",
                                  .nodes = list(size = 10, color = list(background = "lemonchiffon", border = "yellow", highlight = "white"),
                                                font = list(color = "white")),
                                  .edges = list(color = "yellow"),
                                  .options = list(highlightNearest = TRUE,
                                                  nodesIdSelection = list(enabled = TRUE)),
                                  randomSeed = 42)

    dualDrilldownServer("dd",
                        cdata = reactive(mdata$cdata),
                        colorby = colorby,
                        src1 = src1, src2 = src2)

    })
}

#' Launch Shiny app for exploration of relationships in annotated data with an interactive matrix
#'
#' Wrapper to launch app at console
#'
#' @param ns Namespace of app module.
#' @param ... Arguments passed to \code{\link{matrixAppServer}}.
#' @export
matrixApp <- function(ns, ...) {
  ui <- matrixAppUI(ns)
  server <- matrixAppServer(ns, ...)
  shinyApp(ui, server)
}

