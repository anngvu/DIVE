#' Shiny app UI for interactive matrix (v2)
#'
#' See matrixCtrl2UI
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @import shiny
#' @import magrittr
#' @export
matrixApp2UI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {
  ns <- NS(id)
  ns.graph <- paste(ns("graph"), "asNetwork", sep = "-")
  ns.matrix <- paste(ns("matrix"), "matrixMain", sep = "-")
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),
            # Filter controls and data input
            fluidRow(class = "top-panel matrixApp2UI-panel",
                     column(12,
                            div(class = "input-panel-flush",
                              div(style = "padding-left: 50px;",
                                  shinyWidgets::materialSwitch(ns("lookup"), label = "Data Menu")),
                              absolutePanel(id = ns("filter"),
                                            conditionalPanel("input.lookup", ns = ns,
                                              dataHelperUI("metahelper", oneway = TRUE))
                                            )
                            ),
                            div(class = "input-panel", matrixCtrl2UI(ns("ctrl"))),
                            div(class = "input-panel", br(), actionButton(ns("upload"), "Upload my data"))
                            )
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
                     matrixMainUI(ns("matrix"), style = "flex: 4 0 50vw;"),
                     matrixAsNetworkUI(ns("graph"), height = "800px", style = "flex: 4 0 50vw; display: none;"),
                     dualDrilldownUI(ns("dd"))
            )
  )
}

#' Shiny app server for interactive matrix (v2)
#'
#' See matrixCtrl2UI
#'
#' @inheritParams dataUploadServer
#' @inheritParams matrixCtrlServer
#' @inheritParams matrixMainServer
#' @inheritParams matrixAsNetworkServer
#' @inheritParams dualDrilldownServer
#' @param metafilter A reactive filter such as from \code{dataHelper}, used to filter upon the matrix.
#' @import shiny
#' @export
matrixApp2Server <- function(id,
                            M, N, P,
                            cdata,
                            metafilter = reactive({ }),
                            factorx, colorby,
                            colorscales = list(default = list(colorscale_named(pal = "RdBu"), zmin = -1, zmax = 1)),
                            informd = system.file("info/interactive_matrix.Rmd", package = "DIVE"),
                            appdata = NULL) {

  moduleServer(id, function(input, output, session) {

    observeEvent(input$upload, {
      showModal(modalDialog(title = "Upload my data",
                            dataUploadUI(session$ns("upload"), label = NULL),
                            includeMarkdown(informd),
                            footer = modalButton("Cancel")
      ))
    })

    upload <- dataUploadServer("upload",
                               removable = T,
                               checkFun = checkDataUpload,
                               appdata = appdata,
                               checkappdata = T)

    filtervals <- reactiveValues(N = NULL, P = NULL)

    mdata <- matrixCtrl2Server("ctrl",
                               M = M, N = N, P = P,
                               cdata = cdata,
                               metafilter = metafilter,
                               newdata = upload,
                               filtervals = filtervals)

    src1 <- matrixMainServer("matrix",
                             mdata = mdata,
                             colorscales = colorscales)

    src2 <- matrixAsNetworkServer("graph",
                                  mdata,
                                 filtervals,
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
#' @param ... Arguments passed to \code{\link{matrixApp2Server}}.
#' @export
matrixApp2 <- function(ns, ...) {
  ui <- matrixApp2UI(ns)
  server <- matrixApp2Server(ns, ...)
  shinyApp(ui, server)
}

