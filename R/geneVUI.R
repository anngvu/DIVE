#' Shiny module UI for filtering high dimensional genomics data
#'
#' Currently, this provides options to subset genomics data using a custom list input
#' or GO and Reactome annotations.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
geneVUI <- function(id) {
  ns <- NS(id)
  fluidRow(div(class = "forceInline",
               selectizeInput(ns("IDs"), "Genes/proteins of interest",
                              choices = NULL, selected = NULL,
                              options = list(maxItems = 50))),
           div(class = "forceInline", br(),
               actionButton(ns("xlist"), "Quick list", icon = icon("plus"))),
           div(class = "forceInline",
               textInput(ns("qtext"), "", placeholder = "search query...")),
           div(class = "forceInline", br(),
               actionButton(ns("query"), "Query")),
           div(class = "forceInline", br(),
               infoOutput("query", label = "", i = "question-circle"))
           )
}

#' Shiny module server for filtering high dimensional genomics data
#'
#' Currently, this provides options to subset genomics data using custom list input
#' or GO and Reactome annotations. The output can be passed to multiple "tracks"
#' in multiVUI for simultaneous filtering.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param choices Choices for selectInput.
#' @param prelist Optional, a named list of source files that store pre-compiled sets for convenient access.
#' @return A vector that can be used to subset a high dimensional matrix, i.e. the parameter
#' selected in multiV.
#' @export
geneV <- function(input, output, session,
                  choices) {

  selected <- reactiveVal(choices)

  updateSelectizeInput(session, "IDs", "Genes/proteins of interest", choices = choices,
                       selected = character(0), options = list(maxItems = 50), server = T)

  observeEvent(input$xlist, {
    showModal(modalDialog(
      HTML("<strong>Pre-compiled, curated lists</strong><br><li>"),
      getLinkInput(session$ns("precompiled"), labels = "T1Dbase"),
      HTML("<br><br><strong>Upload my custom list</strong><br>"),
      helpText("Your list should be a text file with one gene per line."),
      dataUploadUI("customlist", label = ""),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  prelist <- callModule(getLink, "precompiled", sources = system.file("appdata/t1dbase.txt", package = "DIVE"))

  observeEvent(input$IDs, {
    if(is.null(input$IDs)) selected(choices) else selected(input$IDs)
  }, ignoreNULL = FALSE)

  observeEvent(prelist(), {
    selected(choices[prelist()])
    removeModal()
  })

  observeEvent(input$query, {
    result <- mygene::query(input$qtext, species = "human")
    if(result$total > 0) {
      selected(result$hits$entrezgene)
    } else {
      showModal(modalDialog("No results", easyClose = T))
    }
  })

  return(selected)

}

