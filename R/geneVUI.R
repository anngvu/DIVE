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
  tags$div(id = "geneVUI",
           div(class = "forceInline",
               selectizeInput(ns("IDs"), HTML("<strong>Genes/gene products of interest</strong>"),
                              choices = NULL, selected = NULL,
                              options = list(maxItems = 50, placeholder = "globally filter across all datasets"))),
           div(class = "forceInline", br(),
               actionButton(ns("xlist"), "Quick list", icon = icon("plus"))),
           div(class = "forceInline",
               textInput(ns("qtext"), "", placeholder = "search query...")),
           div(class = "forceInline", br(),
               actionButton(ns("query"), "Query")),
           div(class = "forceInline", br(),
               infoOutput(ns("querytips"), label = "tips", i = "question-circle")),
           div(class = "forceInline", br(),
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
geneV <- function(input, output, session,
                  choices, prelist = NULL) {

  callModule(info, "querytips", infoRmd = system.file("help/query_api.Rmd", package = "DIVE"))

  selected <- reactiveVal(choices)
  querystatus <- reactiveVal("")

  updateSelectizeInput(session, "IDs", "Genes/proteins of interest", choices = choices,
                       selected = character(0), options = list(maxItems = 50), server = T)

  observeEvent(input$xlist, {
    prelistdiv <- if(!is.null(prelist)) {
      tags$div(HTML("<strong>Pre-compiled, curated lists</strong><br><li>"),
               getLinkInput(session$ns("precompiled"), labels = names(prelist)))
    } else {
      NULL
    }

    showModal(modalDialog(
      prelistdiv,
      HTML("<br><br><strong>Upload my custom list</strong><br>"),
      helpText("Your list should be a text file with one gene per line."),
      dataUploadUI("customlist", label = ""),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  prelistgo <- callModule(getLink, "precompiled", sources = prelist)

  observeEvent(input$IDs, {
    if(is.null(input$IDs)) selected(choices) else selected(input$IDs)
  }, ignoreNULL = FALSE)

  observeEvent(prelistgo(), {
    selected(choices[prelistgo()])
    removeModal()
  })

  observeEvent(input$query, {
    withProgress(expr =
    {
      result <- tryCatch({ mygene::query(input$qtext, species = "human") },
                         error = function(e) { return(NA) })
      if(!is.na(result) && result$total > 0) {
        selected(result$hits$entrezgene)
        querystatus("")
      } else {
        querystatus("No results.")
      }
    }, value = 0.5, message = "looking up...")
  })

  output$querystatus <- renderText({
    querystatus()
  })

  return(selected)

}

