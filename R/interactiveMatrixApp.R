#' Launch Shiny app for exploration of relationships in annotated data with an interactive matrix
#'
#' See matrixCtrlUI
#'
#' @param M A data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param N A matrix of the same dimensions as M with data for the filterable layer, e.g. sample size.
#' @param CDATA The non-reactive data used for generating the matrix.
#' @param METADATA A data.table with "Variable" as a key column and any number of columns (metadata) to be used as filters.
#' @param css Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
interactiveMatrixApp <- function(M = cor.data$corM, N = cor.data$corN, CDATA = cdata, METADATA = metadata,
                                 CSS = system.file("App/www/", "app.css", package = "DIVE")) {
    ui <- fluidPage(theme = shinytheme("lumen"), includeCSS(CSS),

                    fluidRow(style="margin-top:50px; margin-bottom:50px; margin-right:100px",
                        column(8,
                               matrixCtrlUI("ctrl"),
                               conditionalPanel("output.usewidget == 1",
                                                absolutePanel(id = "cellpackpanel", draggable = T, left = 300,
                                                              cellPackUI("cellfilter"))
                               )),
                        column(4, dataUploadUI("upload", hasInfo = T))),
                    fluidRow(interactiveMatrixUI("matrix")),
                    fluidRow(verbatimTextOutput("test"))
    )


    server <- function(input, output) {

        cellfilter <- callModule(cellPack, "cellfilter", json = system.file("App/www/", "test.json", package = "DIVE"))
        widget <- reactive({
            input[[cellfilter]]
        })
        upload <- callModule(dataUpload, "upload", removable = T)
        display <- callModule(matrixCtrl, "ctrl", M, N, CDATA, METADATA,
                            newdata = upload, widget = widget)
        matrix <- callModule(interactiveMatrix, "matrix", state = display)
        output$usewidget <- reactive({
            if(display$optgroup == "Cell/Tissue") 1 else 0
        })
        outputOptions(output, "usewidget", suspendWhenHidden = FALSE)
        output$test <- renderPrint({

        })
    }

    # Run the application
    shinyApp(ui = ui, server = server)
}
