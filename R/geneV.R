#' Shiny module UI for select-filter of genes or gene products
#'
#' Augmented select-filter interface for genes
#'
#' @family geneV functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
geneVUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "geneVUI",
           div(class = "ui-inline",
               selectizeInput(ns("IDs"), tags$strong("Genes/gene products of interest"),
                              choices = NULL, selected = NULL,
                              options = list(maxItems = 50, placeholder = "globally filter across all datasets"))),
           div(class = "ui-inline", br(),
               actionButton(ns("qlist"), "Quick list", icon = icon("plus"))),
           div(class = "ui-inline",
               textInput(ns("qtext"), "", placeholder = "search query...")),
           div(class = "ui-inline", br(),
               actionButton(ns("query"), "Query")),
           div(class = "ui-inline", br(),
               infoOutput(ns("querytips"), label = "tips", i = "question-circle")),
           div(class = "ui-inline status-update", br(),
               textOutput(ns("querystatus"), inline = TRUE)),
           helpText("Note: Expression values may not be available in all assays.")
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
#' @param genes Gene choices for initialization in \code{shiny::\link[shiny]{selectizeInput}}.
#' @return A vector of standard gene symbols.
#' @export
geneVServer <- function(id,
                        genes = NULL) {

  moduleServer(id, function(input, output, session) {

    infoServer("querytips", informd = system.file("help/query_api.Rmd", package = "DIVE"))

    selected <- reactiveVal(genes)
    querystatus <- reactiveVal("")

    updateSelectizeInput(session, "IDs", "Genes/proteins of interest", choices = genes,
                         selected = character(0), options = list(maxItems = 50), server = T)

    observeEvent(input$qlist, {
      showModal(modalDialog(
        tags$strong("Upload my custom list"),
        helpText("Your list should be a text file with one gene per line."),
        dataUploadUI("customlist", label = NULL),
        size = "s", easyClose = TRUE, footer = NULL
      ))
    })

    observeEvent(input$IDs, {
      if(is.null(input$IDs)) selected(genes) else selected(input$IDs)
    }, ignoreNULL = FALSE)

    # -- API handlers --------------------------------------------------------------------------#

    observeEvent(input$query, {
      withProgress(expr = {
        result <- tryCatch({
          mygene::query(input$qtext, species = "human")
        }, error = function(e) { return(NA) })
        if(!is.na(result) && result$total > 0) {
          selected(result$hits$entrezgene)
          querystatus("")
        } else {
          querystatus("No results.")
        }
      }, value = 0.5, message = "querying API...")
    })

    output$querystatus <- renderText({
      querystatus()
    })

    return(selected)
  })

}

