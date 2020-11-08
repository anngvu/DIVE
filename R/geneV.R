#' Shiny module UI for select-filter of genes or gene products
#'
#' Augmented select-filter interface for genes
#'
#' @family geneV functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @import shiny
#' @export
geneVUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "geneVUI-panel input-panel theme-border", id = ns("geneVUI"),
           div(class = "ui-inline", style = "width: 500px;",
               textAreaInput(ns("qtext"), tags$strong("Genes/proteins of interest"),
                             placeholder = "enter list of gene IDs (max 1000), genomic range, keyword, concept, ontology term, etc., to globally search across displayed datasets",
                             width = "500px", height = "90px", resize = "none")),
          div(class = "ui-inline",
              infoOutput(ns("querytips"), label = "help/syntax", i = "question-circle"), br(), br(),
              actionButton(ns("query"), "Query", icon = icon("angle-right")),
              div(class = "status-update", textOutput(ns("querystatus")))
          )
  )
}

#' Shiny module server for select-filter of genes or gene products
#'
#' Implement augmented select-filter interface for genes powered by \code{\link{mygene}}
#'
#' With the help of the \code{\link{mygene}} API, this provides an interface for reasonably
#' flexible ways to get select some subset of genes. The first problem for a simple selection is that
#' genomics data often requires mapping across different database IDs and accessions,
#' e.g. between NCBI gene IDs, HUGO gene symbols, Ensembl, or to a protein product (Uniprot accessions),
#' all of which are constantly being updated so that IDs become deprecated/obsolete.
#' The second problem is that, aside from being able to use any database ID, one might want to use
#' genomic coordinates or even Gene Ontology references.
#' The end return should be whatever gene IDs the application requires, but what should be provided to
#' the user to make their selection? This implements three entryways:
#' \itemize{
#'   \item The main select loads the preferred gene IDs stored locally by the application.
#'   \item A query field powered by the \code{\link{mygene}} API to do translations using the most current data.
#'   \item A gene list upload option to upload a gene list (one gene per line).
#' }
#'
#' The return can be passed in to the parameter \code{selected} in the \code{\link{multiVServer}} module.
#'
#' @family geneV functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return A vector of standard gene symbols.
#' @import shiny
#' @export
geneVServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    infoServer("querytips", informd = system.file("info/query_api.Rmd", package = "DIVE"))

    selected <- reactiveVal(NULL)
    querystatus <- reactiveVal("")

    # -- MyGene API handlers ----------------------------------------------------------------------#

    # query for "/gene" service
    processGeneList <- function(q) {
      q <- sub("ids=","", q)
      result <- tryCatch({
        mygene::getGenes(q, fields = "entrezgene", species = "human")
      }, error = function(e) { return(NA) })

      if(!is.na(result) && length(result$entrezgene) > 0) {
        hits <- result$entrezgene
        selected(result$entrezgene)
        querystatus(paste(length(hits), "valid geneids"))
      } else {
        querystatus("no results")
      }
    }

    # process query for "/query?q=<query>" service
    processQuery <- function(q) {
      result <- tryCatch({
          mygene::query(input$qtext, species = "human")
        }, error = function(e) { return(NA) })

        if(!is.na(result) && result$total > 0) {
          hits <- result$hits$entrezgene
          selected(result$hits$entrezgene)
          querystatus(paste(length(hits), "hits"))
        } else {
          querystatus("no results")
        }
    }

    observeEvent(input$query, {
      withProgress(expr = {
        if(startsWith(trimws(input$qtext), "ids=")) processGeneList(input$qtext) else processQuery(input$qtext)
      }, value = 0.5, style = "old", message = "querying...")
    })

    output$querystatus <- renderText({
      querystatus()
    })

    return(selected)
  })

}

