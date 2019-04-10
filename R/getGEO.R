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
#' Handles importing GEO data by guiding user through a number of modals.
#'
#' Returns a reactive values object containing \preformatted{$accession},
#' \preformatted{$eset}, \preformatted{$pData} and \preformatted{$call}.
#' The variable \preformatted{$call} is an internal counter for use with
#' other modules that need to update when new GEO dataset has been processed
#' completely through the annotation step
#' (even when it has the same accession number).
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @return GEOdata as reactive values object. See details.
#' @export
getGEOMod <- function(input, output, session) {

  characteristics <- reactiveVal(NULL)
  GPL <- reactiveVal(NULL)
  GEOdata <- reactiveValues(accession = NULL, eset = NULL, pData = NULL, call = NULL)

  # Pull GEO with given GSE
  observeEvent(input$get, {
    gse <- try(GEOquery::getGEO(trimws(input$GSE)))
    if(class(gse) != "try-error") {
      eset <- gse[[1]]
      xdata <- Biobase::exprs(eset)
      if(!nrow(xdata)) {
        showNotification("Unfortunately, it looks like the accession provides data only as non-standard supplementary file(s),
                         which we don't support.",
                         duration = NULL, type = "error")
      } else {
        GEOdata$accession <- trimws(input$GSE)
        GEOdata$eset <- xdata
        # extra phenotype metadata
        meta <- Biobase::pData(eset)
        charts <- grep(":", names(meta), value = T)
        characteristics(meta[, charts])
        # extract platform annotation
        gpl <- GEOquery::Table(GEOquery::getGEO(Biobase::annotation(eset)))
        GPL(gpl)

        showModal(modalDialog(title = "Step 2",
          selectizeInput(session$ns("characteristics"),
                                    HTML("<strong>Which characteristics do you want to import as relevant clinical/phenotype/experimental data?</strong>"),
                                    choices = charts, selected = charts, multiple = T, width = "100%"),
          helpText("(clear all selections to import none)"),
          actionButton(session$ns("importC"), "OK"),
          br(), br(), h5("Characteristics (preview)"),
          tableOutput(session$ns("pChars")),
          footer = modalButton("Cancel")
        ))
      }
    } else {

    }

  })

  observeEvent(input$importC, {
    showModal(modalDialog(title = "Step 3",
                          selectizeInput(session$ns("annofield"),
                                         HTML("<strong>Which annotation field to use for lookup and filtering?</strong>"),
                                         choices = colnames(GPL()), width = "100%"),
                          helpText("Choosing the column containing Entrez gene ID (NOT symbol) is recommended,
                                   but if it is not available, choose the annotation most useful for you."),
                          actionButton(session$ns("annotate"), "OK"),
                          br(), br(), h5("Annotation table (preview)"),
                          tableOutput(session$ns("gplTable")),
                          footer = modalButton("Cancel")
    ))
  })

  output$pChars <- renderTable({
    head(characteristics())
  }, spacing = "xs")

  output$gplTable <- renderTable({
    head(GPL())
  }, spacing = "xs")

  observeEvent(input$annotate, {
    if(length(input$characteristics)) GEOdata$pData <- characteristics()[, input$characteristics, drop = F]
    rownames(GEOdata$eset) <- as.character(GPL()[, input$annofield])
    GEOdata$call <- input$annotate
  })

  # Checks: species is human, 1-channel data

  # Get metadata

  #

  return(GEOdata)

}
