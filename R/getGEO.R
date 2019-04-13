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

  Step2 <- function() {
    showModal(modalDialog(title = "Step 2",
                          selectizeInput(session$ns("characteristics"),
                                         HTML("<strong>Which characteristics do you want to import as relevant clinical/phenotype/experimental data?</strong>"),
                                         choices = names(characteristics()), selected = names(characteristics()), multiple = T, width = "100%"),
                          helpText("(clear all selections to import none)"),
                          actionButton(session$ns("importC"), "OK"),
                          br(), br(), h5("Characteristics (preview)"),
                          tableOutput(session$ns("pChars")),
                          footer = modalButton("Cancel")
    ))
  }

  Step3 <- function() {
    showModal(modalDialog(title = "Step 3",
                          selectizeInput(session$ns("annofield"),
                                         HTML("<strong>Choose annotation field to use for lookup and filtering?</strong>"),
                                         choices = names(GPL()), width = "100%"),
                          helpText("Choosing the column containing Entrez gene ID (NOT symbol) is recommended,
                                   but if it is not available, choose the annotation most useful for you."),
                          actionButton(session$ns("annotate"), "OK"),
                          br(), br(), h5("Annotation table (preview)"),
                          tableOutput(session$ns("gplTable")),
                          footer = modalButton("Cancel")
    ))
  }


  # Pull GEO with given GSE
  observeEvent(input$get, {
  withProgress(value = 0.2, message = "downloading...", expr = {
    gse <- try(GEOquery::getGEO(trimws(input$GSE)))
    if(class(gse) == "try-error") {
      showNotification("Something went wrong. Try again later or let us know which accession failed.",
                       duration = NULL, type = "error")
    } else {
      setProgress(value = 0.3, message = "checking data...")
      eset <- gse[[1]]
      meta <- Biobase::otherInfo(Biobase::experimentData(eset))
      xdata <- Biobase::exprs(eset)
      pdata <- Biobase::pData(eset)
      characteristics(pdata[, grep(":", names(pdata))])

      if(!nrow(xdata) && grepl("sequencing", meta$type)) {
        setProgress(value = 0.5, message = "sourcing sequencing data...")
        sra <- regmatches(meta$relation, regexpr("(https://www.ncbi.nlm.nih.gov/sra?term=)?SRP[0-9]+", meta$relation))
        recounted <- getRecount(sra)
        if(!is.null(recounted)) {
          xdata <- recounted$xdata
          characteristics(recounted$pdata)
        }
      }

      if(nrow(xdata)) {
        setProgress(value = 0.8, message = "processing data...")
        GEOdata$accession <- trimws(input$GSE)
        GEOdata$eset <- xdata
        gpl <- GEOquery::Table(GEOquery::getGEO(Biobase::annotation(eset)))
        if(!length(gpl)) gpl <- data.frame(gene_id = rownames(xdata)) # when no annotation, use rownames of xdata
        GPL(gpl)
        Step2()
      } else {
        showNotification("Processed data not available.", duration = NULL, type = "error")
      }
    }
  })
  })

  observeEvent(input$importC, {
    Step3()
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

  return(GEOdata)

}

checkGEO <- function(xdata, meta,
                     supported = c("Expression profiling by array",
                       "Expression profiling by high throughput sequencing",
                       "Protein profiling by protein array",
                       "Methylation profiling by array",
                       "Methylation profiling by high throughput sequencing")
                     ) {

  if(!nrow(xdata) & grepl("sequencing", meta$type)) {
    sra <- regmatches(meta$relation, regexpr("(https://www.ncbi.nlm.nih.gov/sra?term=)?SRP[0-9]+", meta$relation))
    data <- getRecount(sra)
    return(data)
  } else {
    return("Unfortunately, no processed data is available and we don't use non-standard supplementary file(s).")
  }
  # if(pdata$channel_count[1] != 1) return("Unfortunately, we don't support visualization of GEO data outside certain platforms/formats (one-channel arrays, RNA-seq).")
}


getRecount <- function(sra) {
  link <- NULL
  try(link <- recount::download_study(sra, download = F), silent = T)
  rse_gene <- NULL
  if(!is.null(link)) {
    attempt <- 0
    while(is.null(rse_gene) && attempt <= 1) {
      attempt <- attempt + 1
      try(load(url(link)), silent = T)
    }
    scaled <- recount::scale_counts(rse_gene)
    xdata <- SummarizedExperiment::assay(scaled)
    pdata <- recount::geo_characteristics(SummarizedExperiment::colData(rse_gene))
    rownames(pdata) <- SummarizedExperiment::colData(rse_gene)$run
    return(list(xdata = xdata, pdata = pdata))
  }
}
