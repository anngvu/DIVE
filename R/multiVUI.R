#' Shiny app UI for multi-views
#'
#' Assembles the UI of various module components, i.e. \code{\link{xVUI}}, \code{\link{geneVUI}},
#' into a working one-page application
#'
#' @family multiVUI module functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
multiVUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),
            shinyWidgets::chooseSliderSkin("Flat"),

            fluidRow(class = "multiVUI-ctrl-head", id = ns("multiVUI-ctrl-head"),
              tags$div(style = "margin-top: 50px; margin-left: 50px;",
              column(1, br(), h4("DATA SOURCES")),
              column(7, multiVCtrlUI(ns("ctrl"))),
              column(1, br(), h4("DATA TOOLS")),
              column(3, br(), br(),
                     div(class = "forceInline", actionButton(ns("newSubgroupVUI"), " Subgroup view", icon = icon("object-ungroup"))),
                     div(class = "forceInline", actionButton(ns("ML"), "Learn", icon = icon("cog")))
              )
            )),
            fluidRow(absolutePanel(style = "z-index: 10;", tags$div(id = "views"), draggable = T)),
            fluidRow(style = "padding-top: 50px;",
              conditionalPanel(condition = paste0("input['", ns("ctrl-dataset"), "']"),
                               column(8, geneVUI(ns("gene"))),
                               column(4, selectVUI(ns("cdata")))
            )),
            div(id = "displaytrack")
  )
}

#' Shiny app server module for multi-views
#'
#' This assembles the logic of various module components into a working one-page application.
#' First, the module calls \code{\link{multiVCtrl}},
#' which returns a named list object which either contains data or NULL.
#' A section is either dynamically added for data or removed for NULL,
#' using the name of the object, which is something like "i1" and corresponds to the
#' index of the stored global datasets. Thus, a section with id containing "-i1" corresponds to
#' a representation of the first dataset. Each section is rendered by \code{\link{xV}}.
#'
#' While \code{\link{multiVCtrl}} is a global control that calls one or more sections into
#' existence on the page, there are two other types of global controls that affect
#' the sections present. These are the \code{\link{geneV}} and \code{\link{selectV}} controls,
#' which function as global filters for attributes that should be present in all or most of the data
#' (for expression matrix data, this means gene/protein and attributes corresponding to samples).
#' Thus, each \code{\link{xV}} section component listens to
#' \code{\link{geneV}} and \code{\link{selectV}} outputs.
#'
#' However, each section also has its own local options that control display of only that section,
#' as seen in \code{\link{xVUI}}.
#'
#' @family multiVUI module functions
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @export
multiV <- function(input, output, session,
                   hdata = NULL,
                   hcat = NULL,
                   cdata = NULL,
                   factorx = NULL,
                   genes = DIVE::gene_symbols,
                   prelist = NULL,
                   slabel = DIVE::gene_symbols_map,
                   preselect = NULL) {

  view <- callModule(multiVCtrl, "ctrl",
                     cdata = cdata,
                     hdlist = hdata,
                     choices = hcat,
                     factorx = factorx,
                     preselect = preselect)

  # controls clinical/phenotype/other variable selection for all xVUI components
  vselect <- callModule(selectV, "cdata",
                        data = reactive(view$cdata),
                        selected = reactive(view$vselect),
                        countby = reactive(view$hdata))

  # controls gene selection for all xVUI components
  gselect <- callModule(geneV, "gene",
                        choices = genes,
                        prelist = prelist)

  # each dataset gets its own section with its own xVUI local module options
  observeEvent(view$hdata, {
    trackID <- session$ns(names(view$hdata))
    trackdata <- view$hdata[[1]]
    if(!is.null(trackdata)) {
      insertUI(selector = "#displaytrack", immediate = T,
               ui = tags$div(id = trackID, xVUI(id = trackID)))
      callModule(xV, id = names(view$hdata),
                 hdata = trackdata,
                 cdata = vselect,
                 selected = gselect,
                 slabel = slabel)
    } else {
      removeUI(selector = paste0("#", trackID))
    }
  })

  # data tools
  observeEvent(input$newSubgroupVUI, {
    N <- input$newSubgroupVUI
    insertUI(paste0("#views"),
             ui = subgroupVUI(id = session$ns(paste0("panel", N))))
    callModule(subgroupV, id = paste0("panel", N),
               cdata = view$cdata,
               hdlist = view$hdlist)
  })


}

#' Shiny app launcher for multi-view module
#'
#' @family multiVUI module functions
#'
#' @export
multiVUIR <- function(ns, ...) {
  ui <- multiVUI(ns)
  server <- function(input, output, session) { callModule(multiV, ns, ...) }
  shinyApp(ui = ui, server = server)
}
