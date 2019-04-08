#' Shiny module UI for importing data from GEO
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
getGEOInput <- function(id, infoRmd = system.file("help/GEO_module.Rmd", package = "DIVE")) {
  ns <- NS(id)
  tags$div(id = "getGEOInput",
           div(class = "forceInline", textInput(ns("GSE"), label = "Enter a GEO accession, e.g. 'GSE72492'")),
           div(class = "forceInline", br(), actionButton(ns("get"), "", icon = icon("arrow-right"))),
           includeMarkdown(infoRmd)
           )
}

#' Shiny module server for importing data from GEO
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @export
getGEOMod <- function(input, output, session) {

  characteristics <- reactiveVal(NULL)
  GEOdata <- reactiveValues(accession = NULL, eset = NULL, pData = NULL)

  # Pull GEO with given GSE
  observeEvent(input$get, {
    gse <- try(GEOquery::getGEO(trimws(input$GSE)))
    if(class(gse) != "try-error") {
      eset <- gse[[1]]
      GEOdata$eset <- Biobase::exprs(eset)
      meta <- Biobase::pData(eset)
      charts <- grep(":", names(meta), value = T)
      characteristics(meta[, charts])
      showModal(modalDialog(title = "Step 2",
        selectizeInput(session$ns("characteristics"),
                                  HTML("<strong>Which characteristics do you want to import as relevant clinical/phenotype/experimental data?</strong>"),
                                  choices = charts, selected = charts, multiple = T, width = "100%"),
        helpText("(clear all selections to import none)"),
        actionButton(session$ns("selectC"), "Import"),
        br(), br(), h5("Sample characteristics preview"),
        tableOutput(session$ns("pChars")),
        footer = modalButton("Cancel")
      ))
    } else {

    }

  })

  output$pChars <- renderTable({
    head(characteristics())
  }, spacing = "xs")

  observeEvent(input$selectC, {
    if(length(input$characteristics)) GEOdata$pData <- characteristics()[, input$characteristics, drop = F]
    GEOdata$accession <- trimws(input$GSE)
  })

  # Checks: species is human, 1-channel data

  # Get metadata

  #

  return(GEOdata)

}
