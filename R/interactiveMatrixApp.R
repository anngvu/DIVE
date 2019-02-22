# 
# metadata <- fread("metadata.csv")
# load("correlations.Rdata")
# load("cdata.Rdata")

#' Launch Shiny app for exploration of relationships in annotated data with an interactive matrix
#' 
#' See matrixCtrlUI
#'
#' @param M A data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param N A matrix of the same dimensions as M with data for the filterable layer, e.g. sample size.
#' @param cdata The non-reactive data used for generating the matrix.
#' @param metadata A data.table with "Variable" as a key column and any number of columns (metadata) to be used as filters.
#' @export
interactiveMatrixApp <- function(M, N, cdata, metadata) {
    ui <- fluidPage(theme = shinytheme("lumen"), tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
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
    
        cellfilter <- callModule(cellPack, "cellfilter", json = "www/test.json")
        widget <- reactive({
            input[[cellfilter]]
        })
        upload <- callModule(dataUpload, "upload", removable = T)
        state <- callModule(matrixCtrl, "ctrl", M = cor.data$corM, N = cor.data$corN, cdata = cdata,
                            metadata = metadata, newdata = upload, widget = widget)
        matrix <- callModule(interactiveMatrix, "matrix", state = state)
        output$usewidget <- reactive({
            if(state$optgroup == "Cell/Tissue") 1 else 0
        })
        outputOptions(output, "usewidget", suspendWhenHidden = FALSE)
        output$test <- renderPrint({
          
        })
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
}