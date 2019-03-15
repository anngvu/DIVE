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
            div(id = "displaytrack")
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
  
  track <- 0 # 
  selected <- callModule(geneV, "gene", choices = CHOICES)
  
  view <- callModule(multiVCtrl, "ctrl", hdlist = list("genomics1" = xm_t, "px1" = px1_t, "px2" = px2_t))
  
  observeEvent(view(), {
    track <<- track + 1
    insertUI(selector = "#displaytrack", immediate = T,
             ui = multiVUI(id = session$ns(paste0("track", track))))
    callModule(multiV, paste0("track", track), hdata = isolate(view()), cdata = CDATA, selected = selected)
  })
  
}

multiVAppRun <- function() {
  ui <- multiVAppUI("default")
  server <- function(input, output, session) { callModule(multiVApp, "default") }
  shinyApp(ui = ui, server = server)
}