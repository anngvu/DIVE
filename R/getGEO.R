#' Shiny module UI for importing data from GEO
#'
#' Interactively import GEO data with step-by-step process using a series of modals
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
getGEOInput <- function(id, informd = system.file("help/GEO_module.Rmd", package = "DIVE")) {
  ns <- NS(id)
  tags$div(id = ns("get-GEO-input"),
           div(class = "ui-inline", textInput(ns("GSE"), label = "Enter a GEO accession, e.g. 'GSE72492'")),
           div(class = "ui-inline", br(), actionButton(ns("get"), "", icon = icon("arrow-right"))),
           includeMarkdown(informd)
           )
}

#' Shiny module server for importing data from GEO
#'
#' Interactively import GEO data with step-by-step process using a series of modals
#'
#' This server function implements interactive GEO data import with three main steps.
#' \enumerate{
#'   \item Query for accession, extract expression data and check whether of compatible type and format.
#'   \item Import characteristics data selectively according to user.
#'   \item Import the feature annotation according to user.
#' }
#' Returns a reactive values object containing \code{$accession},
#' \code{$eset}, \code{$pData} and \code{$return}.
#' \code{$return} is an internal status flag for use by \code{\link{multiVCtrlServer}}
#' to know when new GEO dataset has been processed completely through the annotation step.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return GEOdata as reactive values object. See details.
#' @export
getGEOServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    characteristics <- reactiveVal(NULL)
    GPL <- reactiveVal(NULL)
    GEOdata <- reactiveValues(accession = NULL, eset = NULL, pData = NULL, return = NULL)

    # (Step 1) Pull GEO with given GSE
    observeEvent(input$get, {
      withProgress(value = 0.2, message = "downloading...", expr = {
        gse <- try(GEOquery::getGEO(trimws(input$GSE)))
        if(class(gse) == "try-error") {
          showNotification("Something went wrong. Try again later or let us know which accession failed.",
                           duration = NULL, type = "error")
        } else {
          setProgress(value = 0.3, message = "extracting data...")
          eset <- gse[[1]]
          meta <- Biobase::otherInfo(Biobase::experimentData(eset))
          xdata <- Biobase::exprs(eset)
          pdata <- Biobase::pData(eset)
          characteristics(pdata[, grep(":", names(pdata))])

          if(nrow(xdata)) {
            setProgress(value = 0.8, message = "processing data...")
            GEOdata$accession <- trimws(input$GSE)
            GEOdata$eset <- xdata
            gpl <- GEOquery::Table(GEOquery::getGEO(Biobase::annotation(eset)))
            if(!length(gpl)) gpl <- data.frame(gene_id = rownames(xdata)) # when no annotation, use rownames of xdata
            GPL(gpl)
            Step2()
          } else {
            showNotification("Standardized matrix data from GEO is not available.", duration = NULL, type = "error")
          }
        }
      })
    })

    # (Step 2) Selecting characteristics to import from pData
    Step2 <- function() {
      showModal(
        modalDialog(title = "Step 2",
                    selectizeInput(session$ns("selectpdata"),
                                   HTML("<strong>Which do you want to import as relevant phenotype/experimental data?</strong>"),
                                   choices = names(characteristics()), selected = names(characteristics()),
                                   multiple = T, width = "100%"),
                    helpText("(clear all selections to import none)"),
                    actionButton(session$ns("importChars"), "OK"),
                    br(), br(), tags$em("Characteristics (preview)"),
                    tableOutput(session$ns("pChars")),
                    footer = modalButton("Cancel")
        )
      )
    }

    # (Step 3) Selecting which annotation field to use for searching/filtering of expression matrix columns
    Step3 <- function() {
      showModal(
        modalDialog(title = "Step 3",
                    selectizeInput(session$ns("annofield"),
                                   HTML("<strong>Choose annotation field to use for lookup and filtering</strong>"),
                                   choices = names(GPL()), width = "100%"),
                    helpText("Choosing the column containing Entrez gene ID is recommended."),
                    actionButton(session$ns("annotate"), "OK"),
                    br(), br(), h5("Annotation table"),
                    tableOutput(session$ns("gplTable")),
                    footer = modalButton("Cancel")
        )
      )
    }


    observeEvent(input$importChars, {
      Step3()
    })

    # Render outputs for Step 3
    output$pChars <- renderTable({
      head(characteristics())
    }, spacing = "xs")

    output$gplTable <- renderTable({
      head(GPL())
    }, spacing = "xs")


    # When user proceeds with annotations
    observeEvent(input$annotate, {
      if(length(input$selectpdata)) GEOdata$pData <- characteristics()[, input$selectpdata, drop = F]
      # Map probe IDs (rownames) to Entrez Gene IDs or selected annotation
      probes <- rownames(GEOdata$eset)
      # A column named "ID" *should* exist in gpl table, but check and do alternative annotation otherwise
      IDcol <- "ID"
      annrows <- GPL()[[input$annofield]][match(probes, GPL()[[IDcol]])]
      rownames(GEOdata$eset) <- annrows
      GEOdata$return <- TRUE
    })

    return(GEOdata)
  })

}

# Helper functions -----------------------------------------------------------------------------------------------#
checkGEO <- function(xdata, meta,
                     supported = c("Expression profiling by array",
                                   "Expression profiling by high throughput sequencing",
                                   "Protein profiling by protein array",
                                   "Methylation profiling by array",
                                   "Methylation profiling by high throughput sequencing")
                     ) {

  if(!nrow(xdata) && grepl("sequencing", meta$type)) {
    sra <- regmatches(meta$relation, regexpr("(https://www.ncbi.nlm.nih.gov/sra?term=)?SRP[0-9]+", meta$relation))
    data <- getRecount(sra)
    return(data)
  } else {
    return("Unfortunately, no processed data is available and we don't use non-standard supplementary file(s).")
  }
  # if(pdata$channel_count[1] != 1) return("Unfortunately, we don't support visualization of GEO data outside certain platforms/formats (one-channel arrays, RNA-seq).")
}


fromRecount <- function() {
  if(!is.null(recounted)) {
    xdata <- recounted$xdata
    characteristics(recounted$pdata)
  }
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
