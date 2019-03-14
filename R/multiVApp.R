#' Shiny app UI for multi-views
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
multiVAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {
  
  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("lumen"), includeCSS(CSS),
            geneVUI(ns("main")),
            multiVUI(ns("track"))
  )
}

#' Shiny app server for multi-views
#' 
#' Assembles various components into a one-page application
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @export
multiVApp <- function(input, output, session,
                      HDATA = xm_t, CDATA = cdata, CHOICES = gene_symbols) {
  
  selected <- callModule(geneV, "main", choices = CHOICES)
  mv <- callModule(multiV, "track", hdata = HDATA, cdata = CDATA, selected = selected)
  
}

multiVAppRun <- function() {
  ui <- multiVAppUI("default")
  server <- function(input, output, session) { callModule(multiVApp, "default") }
  shinyApp(ui = ui, server = server)
}