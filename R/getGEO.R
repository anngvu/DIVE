#' Shiny module UI for importing data from GEO
#'
#' Interactively import GEO data with step-by-step process using a series of modals
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param informd Optional path to Rmarkdown file containing help-text or similar to displayed.
#' @export
getGEOInput <- function(id, informd = system.file("help/GEO_module.Rmd", package = "DIVE")) {
  ns <- NS(id)
  tags$div(id = ns("get-GEO-input"),
           div(class = "ui-inline", textInput(ns("GSE"), label = "Enter a GEO accession, e.g. 'GSE72492'")),
           div(class = "ui-inline", br(), actionButton(ns("get"), "", icon = icon("arrow-right"))),
           if(!is.null(informd)) includeMarkdown(informd)
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
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return \code{GEOdata} reactive values list object containing
#' \code{$accession}, \code{$eset}, \code{$pData} and \code{$return}.
#' \code{$return} is an internal status flag for use by \code{\link{multiVCtrlServer}}
#' to know when a new GEO dataset has been processed completely through the annotation step.
#' @export
getGEOServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    characteristics <- reactiveVal(NULL)
    GPL <- reactiveVal(NULL)
    GEOdata <- reactiveValues(accession = NULL, eset = NULL, pData = NULL, return = NULL)

    # (Step 1) Pull GEO with given GSE
    observeEvent(input$get, {
      withProgress(value = 0.2, message = "downloading...", expr = {
        tryCatch({
          accession <- trimws(input$GSE)
          gse <- try(GEOquery::getGEO(accession)) # !potential error here
          if(inherits(gse, "try-error")) stop("Download failed. Check accession, connection, or try again later.", call. = FALSE)

          setProgress(value = 0.5, message = "checking data...")
          eset <- gse[[1]]
          checkGEOwrapper(eset) # !potential error here

          setProgress(value = 0.8, message = "setting up data...")
          xdata <- Biobase::exprs(eset)
          pdata <- Biobase::pData(eset)
          pdata <- pdata[, grep(":", names(pdata))]
          characteristics(pdata)
          GEOdata$accession <- accession
          GEOdata$eset <- xdata
          gpl <- GEOquery::Table(GEOquery::getGEO(Biobase::annotation(eset)))
          if(!length(gpl)) gpl <- data.frame(gene_id = rownames(xdata)) # when no annotation, use rownames of xdata
          GPL(gpl)
          Step2()
        }, error = function(e) meh(msg = e$message, error = e))
      })
    })

    # (Step 2) Selecting characteristics to import from pData
    Step2 <- function() {
      showModal(
        modalDialog(title = "Step 2",
                    selectizeInput(session$ns("selectpdata"),
                                   tags$strong("Which phenotype/experimental data do you optionally want to import?"),
                                   choices = names(characteristics()), selected = names(characteristics()),
                                   multiple = T, width = "100%",
                                   options = list(placeholder = "none selected")),
                    tags$em("sample characteristics data (preview)"),
                    tags$div(style = "overflow-x: scroll", tableOutput(session$ns("pChars"))),
                    footer = tagList(actionButton(session$ns("importChars"), "OK"), modalButton("Cancel"))
        )
      )
    }

    # (Step 3) Selecting which annotation field to use for searching/filtering of expression matrix columns
    Step3 <- function() {
      showModal(
        modalDialog(title = "Step 3",
                    selectizeInput(session$ns("selectanno"),
                                   tags$strong("Choose annotation field to use for lookup and filtering"),
                                   choices = names(GPL()), width = "100%"),
                    helpText("Choosing the column containing Entrez gene ID is recommended."),
                    tags$em("annotation table"),
                    tags$div(style = "overflow-x: scroll", tableOutput(session$ns("gplTable"))),
                    footer = tagList(actionButton(session$ns("annotate"), "OK"), modalButton("Cancel"))
        )
      )
    }


    observeEvent(input$importChars, {
      Step3()
    })

    # For Step 2
    output$pChars <- renderTable({
      if(length(characteristics())) head(characteristics())
    }, spacing = "xs")

    # For Step 3
    output$gplTable <- renderTable({
      if(length(GPL())) head(GPL())
    }, spacing = "xs")


    # When user proceeds with annotations
    observeEvent(input$annotate, {
      if(length(input$selectpdata)) GEOdata$pData <- characteristics()[, input$selectpdata, drop = F]
      # Map probe IDs (rownames) to Entrez Gene IDs or selected annotation
      probes <- rownames(GEOdata$eset)
      # A column named "ID" *should* exist in gpl table, but check and do alternative annotation otherwise
      IDcol <- "ID"
      annrows <- GPL()[[input$selectanno]][match(probes, GPL()[[IDcol]])]
      rownames(GEOdata$eset) <- annrows
      GEOdata$return <- TRUE
      removeModal()
    })

    return(GEOdata)
  })

}

# Helper functions -----------------------------------------------------------------------------------------------#

#' Check GEO metadata for supported dataset type
#'
#' Check GEO metadata for supported dataset type
#'
#' For reference: https://www.ncbi.nlm.nih.gov/gds/advanced/ -> select field "Dataset Type" -> show index list
#' @family checkGEO functions
checkGEOmetatype <- function(eset,
                             supported = c("Expression profiling by array",
                                           "Expression profiling by high throughput sequencing",
                                           "Protein profiling by protein array",
                                           "Methylation profiling by array",
                                           "Methylation profiling by high throughput sequencing")
                     ) {
  meta <- Biobase::otherInfo(Biobase::experimentData(eset))
  if(meta$type %in% supported) TRUE else FALSE
}

#' Check the GEO series matrix file
#'
#' Check the GEO series matrix file
#'
#' Accessions may not contain data in the matrix file but in the supplement section as an Excel file,
#' which means data cannot by imported.
#' @family checkGEO functions
checkGEOmatrixfile <- function(eset) {
  if(nrow(eset)) TRUE else FALSE
}

#' Check the GEO platform
#'
#' Check the GEO platform
#'
#' Within the broad dataset types, handled by \code{\link{checkGEOmetatype}},
#' the different types of platforms that need to be considered for compatibility.
#' This excludes older platforms that are not one-channel array
#' for the "Expression profiling by array" type by using information in \code{pData},
#' though there might be a better way to do this check.
#' @family checkGEO functions
checkGEOplatform <- function(eset) {
  pdata <- Biobase::pData(eset)
  if(pdata$channel_count[1] != 1) FALSE else TRUE
}


#' Check a GEO dataset for compatibility
#'
#' Check a GEO dataset for compatibility
#'
#' Primarily used in \code{\link{getGEOServer}}, this wraps multiple checks of varying complexity
#' that were compartmentalized to help iteration of adaptable and reliable check steps over time.
#' @family checkGEO functions
checkGEOwrapper <- function(eset) {
  assertthat::assert_that(checkGEOmetatype(eset), msg = "The accession is not of supported dataset type.")
  assertthat::assert_that(checkGEOmatrixfile(eset), msg = "The accession does not contain a standard matrix file.")
  assertthat::assert_that(checkGEOplatform(eset), msg = "The accession dataset is from an incompatible platform.")
}


attemptRecount <- function(meta) {
  sra <- regmatches(meta$relation, regexpr("(https://www.ncbi.nlm.nih.gov/sra?term=)?SRP[0-9]+", meta$relation))
  data <- getRecount(sra)
  return(data)
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

fromRecount <- function() {
  if(!is.null(recounted)) {
    xdata <- recounted$xdata
    characteristics(recounted$pdata)
  }
}
