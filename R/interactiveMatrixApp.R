#' Shiny app UI for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
interactiveMatrixAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {
  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),

            fluidRow(style="margin-top:30px; margin-bottom:20px; margin-right:100px",
                     column(8,
                            matrixCtrlUI(ns("ctrl")),
                            conditionalPanel(paste0("output['", ns("usewidget"), "'] ", "== 1"),
                                             absolutePanel(id = "cellpackpanel", draggable = T, left = 300,
                                                           cellPackUI(ns("cellfilter")))
                            )),
                     column(4, dataUploadUI(ns("upload")))
            ),
            fluidRow(interactiveMatrixUI(ns("matrix")))
  )
}

#' Shiny app server for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param M A data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param N A matrix of the same dimensions as M with data for the filterable layer, e.g. sample size.
#' @param CDATA The non-reactive data used for generating the matrix.
#' @param METADATA A data.table with "Variable" as a key column and any number of columns (metadata) to be used as filters.
#' @export
interactiveMatrixApp <- function(input, output, session,
                                 M = cordata$corM, N = cordata$corN, CDATA = cdata, METADATA = metadata,
                                 widgetopt = "Cell/Tissue",
                                 widgetdata = system.file("www/", "test.json", package = "DIVE"),
                                 infoRmd = "help/data_exploration.Rmd",
                                 appdata = "pilot.csv") {

  cellfilter <- callModule(cellPack, "cellfilter",
                           json = widgetdata)

  upload <- callModule(dataUpload, "upload",
                       removable = T,
                       checkFun = checkForID,
                       infoRmd,
                       appdata)

  display <- callModule(matrixCtrl, "ctrl", M, N, CDATA, METADATA,
                        newdata = upload, widget = cellfilter)

  matrix <- callModule(interactiveMatrix, "matrix", state = display)

  output$usewidget <- reactive({ if(display$optgroup == widgetopt) 1 else 0 })

  outputOptions(output, "usewidget", suspendWhenHidden = FALSE)
}

#' Launch Shiny app for exploration of relationships in annotated data with an interactive matrix
#'
#' Wrapper to launch app at console
#'
#' @export
interactiveMatrixAppRun <- function() {
  ui <- interactiveMatrixAppUI("default")
  server <- function(input, output, session) {
    callModule(interactiveMatrixApp, "default")
  }
  shinyApp(ui = ui, server = server)
}

