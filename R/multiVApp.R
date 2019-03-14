#' Shiny app UI for multi-views
#' 
#' Assembles the UI of various module components, i.e. \code{\link{multiVUI}}, \code{\link{geneVUI}},
#' into a working one-page application
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
multiVAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {
  
  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("lumen"), includeCSS(CSS),
            multiVCtrlUI(ns("ctrl")),
            geneVUI(ns("gene")),
            div(id = "multiVUI-track")
  )
}

#' Shiny app server for multi-views
#' 
#' Assembles the logic of various module components, i.e. \code{\link{multiV}}, \code{\link{geneV}},
#' into a working one-page application
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @export
multiVApp <- function(input, output, session,
                      HDATA = xm_t, CDATA = cdata, CHOICES = gene_symbols) {
  
  selected <- callModule(geneV, "gene", choices = CHOICES)
  
  view <- callModule(multiVCtrl, "ctrl")
  observeEvent(view(), {
    insertUI(selector = "multiVUI-track", multiVUI(session$ns("track")))
    callModule(multiV, session$ns("track"), hdata = view(), cdata = CDATA, selected = selected)
  }
  
}

multiVAppRun <- function() {
  ui <- multiVAppUI("default")
  server <- function(input, output, session) { callModule(multiVApp, "default") }
  shinyApp(ui = ui, server = server)
}