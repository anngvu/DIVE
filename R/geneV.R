#' Shiny module UI for filtering high dimensional genomics data
#'
#' Currently, this provides options to subset genomics data using a custom list input
#' or GO and Reactome annotations.
#'
#' @family multiVUI module functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
geneVUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "geneVUI",
           div(class = "ui-inline",
               selectizeInput(ns("IDs"), HTML("<strong>Genes/gene products of interest</strong>"),
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
           div(class = "ui-inline", br(),
               textOutput(ns("querystatus"))),
           helpText("Note: Expression values may not be available in all assays.")
          )
}

#' Shiny module server for filtering high dimensional genomics data
#'
#' Currently, this provides options to subset genomics data using custom list input
#' or GO and Reactome annotations. The output can be passed to multiple "tracks"
#' in multiVUI for simultaneous filtering.
#'
#' @family multiVApp module functions
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param choices Choices for selectInput.
#' @param prelist Optional, a named list of source files that store pre-compiled sets for convenient access.
#' @return A vector that can be used to subset a high dimensional matrix,
#' which should be passed in to the parameter \code{selected} in the \code{\link{multiV}} server module.
#' @export
geneVServer <- function(id,
                        choices) {

  moduleServer(id, function(input, output, session) {

    infoServer("querytips", informd = system.file("help/query_api.Rmd", package = "DIVE"))

    selected <- reactiveVal(choices)
    querystatus <- reactiveVal("")

    updateSelectizeInput(session, "IDs", "Genes/proteins of interest", choices = choices,
                         selected = character(0), options = list(maxItems = 50), server = T)

    observeEvent(input$qlist, {
      showModal(modalDialog(
        HTML("<br><br><strong>Upload my custom list</strong><br>"),
        helpText("Your list should be a text file with one gene per line."),
        dataUploadUI("customlist", label = NULL),
        easyClose = TRUE, footer = NULL
      ))
    })


    observeEvent(input$IDs, {
      if(is.null(input$IDs)) selected(choices) else selected(input$IDs)
    }, ignoreNULL = FALSE)


    observeEvent(input$query, {
      withProgress(expr =
      {
        result <- tryCatch({
          mygene::query(input$qtext, species = "human")
          }, error = function(e) { return(NA) }
          )
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

