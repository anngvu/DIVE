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

            fluidRow(style="margin-top:30px; margin-bottom:20px;",
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
                                 M = cordata$M,
                                 N = cordata$N,
                                 CDATA = cdata,
                                 METADATA = metadata[, .(VarID, Variable, Contributor, CellTissue, ThemeTag, MethodID)],
                                 widgetopt = "CellTissue",
                                 widgetdata = system.file("www/", "test.json", package = "DIVE"),
                                 infoRmd = system.file("help/interactive_matrix.Rmd", package = "DIVE"),
                                 appdata = "pilot.csv") {

  cellfilter <- callModule(cellPack, "cellfilter",
                           json = widgetdata)

  upload <- callModule(dataUpload, "upload",
                       removable = T,
                       checkFun = checkForID,
                       infoRmd = infoRmd,
                       appdata = appdata)

  display <- callModule(matrixCtrl, "ctrl",
                        M, N, CDATA, METADATA,
                        newdata = upload, widget = cellfilter)

  matrix <- callModule(interactiveMatrix, "matrix", state = display,
                       dcolors = list(
                         donor.type =
                         c("Autoab Pos" = "orange", "Cystic fibrosis" = "aquamarine4", "Transplant" = "gold",
                           "Gastric Bypass" = "bisque4", "Gestational diabetes" = "deeppink2", "Pregnancy" = "pink",
                           "Monogenic Diabetes" = "indianred4", "Fulminant" = "yellow", "Other-Diabetes" = "brown",
                           "Other-No Diabetes" = "steelblue2", "No diabetes" = "dodgerblue",
                           "T1D" = "red", "T1D Medalist" = "maroon", "T2D" = "purple", "Pending" = "gray")
                         # sex = c(Male = "steelblue", Female = "deeppink2")
                         )
                       )

  output$usewidget <- reactive({ if(display$optrowgroup == widgetopt) 1 else 0 })

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

