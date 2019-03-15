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
               selectInput(ns("GO"), "Use Gene Ontology term set", choices = NULL))
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
      HTML("<strong>Pre-compiled lists</strong><br><li>"),
      actionLink("T1Dbase", "T1Dbase genes"),
      HTML("<br><br><strong>Upload list</strong><br>"),
      helpText("Text file should have one gene per line"),
      fileInput(ns("upload"), "", multiple = FALSE, width = "300px",
                accept = c("text/plain"),
                buttonLabel = "My list"),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$IDs, {
    selected(input$IDs)
  })



  return(selected)

}

