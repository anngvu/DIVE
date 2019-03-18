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
                      HDATA = list("genomics1" = xm_t, "px1" = px1_t, "px2" = px2_t), 
                      CDATA = cdata, 
                      CHOICES = gene_symbols) {
  
  selected <- callModule(geneV, "gene", choices = CHOICES) # controls selection for all multiVUIs
  
  view <- callModule(multiVCtrl, "ctrl", hdlist = HDATA)
  
  # each dataset gets its own track (row), served by its own multiVUI module
  observeEvent(view(), {
    trackID <- session$ns(names(view()))
    if(!is.null(view()[[1]])) {
      insertUI(selector = "#displaytrack", immediate = T,
               ui = tags$div(id = trackID, multiVUI(id = trackID)))
      callModule(multiV, id = names(view()), hdata = isolate(view()[[1]]), cdata = CDATA, selected = selected)
    } else {
      removeUI(selector = paste0("#", trackID))
    }
  })
  
}

multiVAppRun <- function() {
  ui <- multiVAppUI("default")
  server <- function(input, output, session) { callModule(multiVApp, "default") }
  shinyApp(ui = ui, server = server)
}