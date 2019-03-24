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
  fluidPage(theme = shinythemes::shinytheme("paper"), includeCSS(CSS),
            fluidRow(style = "background: WhiteSmoke;",
              column(1, br(), h4("DATA SOURCES")),
              column(7, multiVCtrlUI(ns("ctrl"))),
              column(1, br(), h4("DATA TOOLS")),
              column(3, br(), br(),
                     actionButton(ns("newSubgroupVUI"), " Subgroup view", icon = icon("object-ungroup")),
                     absolutePanel(style = "z-index: 10;", tags$div(id = "views"),
                                   top = 100, right = 400, width = 1000, draggable = T))
            ),
            fluidRow(style = "padding-top: 50px;",
              conditionalPanel(condition = paste0("input['", ns("ctrl-dataset"), "']"),
                               column(8, geneVUI(ns("gene"))),
                               column(4, selectVUI(ns("cdata"))))
              ),
            div(id = "displaytrack"),
            div(verbatimTextOutput(ns("test")))
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

  observeEvent(input$newSubgroupVUI, {
    N <- input$newSubgroupVUI
    insertUI(paste0("#views"),
             ui = subgroupVUI(id = session$ns(paste0("panel", N) )) )
    callModule(subgroupV, id = paste0("panel", N) )
  })

  view <- callModule(multiVCtrl, "ctrl", hdlist = HDATA)

  vselect <- callModule(selectV, "cdata", data = CDATA, selected = "donor.type")

  gselect <- callModule(geneV, "gene", choices = CHOICES) # controls selection for all multiVUIs

  # output$test <- renderPrint({
  #
  # })

  # each dataset gets its own track (row), served by its own multiVUI module
  observeEvent(view(), {
    trackID <- session$ns(names(view()))
    trackdata <- view()[[1]]
    if(!is.null(trackdata)) {
      insertUI(selector = "#displaytrack", immediate = T,
               ui = tags$div(id = trackID, style = paste0("height:", 30 * nrow(trackdata), "px"), multiVUI(id = trackID)))
      callModule(multiV, id = names(view()), hdata = isolate(trackdata), cdata = vselect, selected = gselect)
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
