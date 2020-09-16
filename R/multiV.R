#' Shiny app UI for multi-views
#'
#' Assembles the UI of various module components, i.e. \code{\link{xVUI}}, \code{\link{geneVUI}},
#' into a working one-page application
#'
#' @family multiV functions
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
                     div(class = "forceInline",
                         actionButton(ns("newSubgroupVUI"), " Subgroup view", icon = icon("object-ungroup"))),
                     div(class = "forceInline",
                         actionButton(ns("ML"), "Learn", icon = icon("cog")))
              )
            )),
            fluidRow(absolutePanel(style = "z-index: 10;", tags$div(id = "views"), draggable = T)),
            fluidRow(style = "padding-top: 50px;",
                    column(9, geneVUI(ns("gene"))),
                    column(3, selectVUI(ns("cdata")))
            ),
            div(id = "displaytrack")
  )
}

#' Shiny app server module for multi-views
#'
#' Assemble various module components into a working one-page application for expression data
#'
#' First, the server function calls \code{\link{multiVCtrlServer}},
#' which returns a named list object which either contains data or \code{NULL}.
#' If there is data, a \code{\link{xVServer}} is dynamically initiated to render data in its own container.
#' If \code{NULL}, the function removes the appropriate container using the name of the object,
#' which is something like "i1" and corresponds to the index of the stored global datasets.
#'
#' While \code{\link{multiVCtrlServer}} is a global control that controls which datasets are displayed at all,
#' the \code{\link{geneVServer}} and \code{\link{selectVServer}} modules are global controls
#' that modify the display of any displayed datasets through applying global filters on attributes
#' that should be present in all or most of the data
#' (for expression matrix data, this means gene/protein and attributes corresponding to samples).
#' Thus, each \code{\link{xVServer}} component necessarily listens to
#' \code{\link{geneVServer}} and \code{\link{selectVServer}}, but each can also have its own
#' indepedent local controls, which takes precedence if enabled.
#'
#' @family multiVUI functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @inheritParams multiVCtrlServer
#' @inheritParams selectVServer
#' @inheritParams geneVServer
#' @export
multiVServer <- function(id,
                         hdlist = NULL,
                         choices = NULL,
                         cdata = NULL,
                         preselect = NULL,
                         genes = DIVE::gene_symbols) {

  moduleServer(id, function(input, output, session) {

    view <- multiVCtrlServer("ctrl",
                             hdlist = hdlist,
                             choices = choices,
                             cdata = cdata,
                             preselect = preselect)

    # controls clinical/phenotype/other variable selection for all xVUI components
    vselect <- selectVServer("cdata",
                            data = reactive(view$cdata),
                            selected = reactive(view$vselect),
                            countby = reactive(view$hdata))

    # controls gene selection for all xVUI components
    gselect <- geneVServer("gene", choices = genes)

    # each dataset gets its own section with its own xVUI local module options
    obs_view <- observeEvent(view$hdata, {
        trackID <- session$ns(names(view$hdata))
        trackdata <- view$hdata[[1]]
        if(!is.null(trackdata)) {
          height <- if(nrow(trackdata) <=10) { 400 } else { 25 * nrow(trackdata) } # used across plots, tracks
          insertUI(selector = "#displaytrack", immediate = T,
                   ui = tags$div(id = trackID, class = "xV-container", style = paste0("min-height: ", height+30, "px ;"),
                                 xVUI(id = trackID)))
          xVServer(id = names(view$hdata),
                   hdata = trackdata,
                   cdata = vselect,
                   selected = gselect,
                   height = height)
        } else {
          removeUI(selector = paste0("#", trackID))
        }
    }, ignoreInit = TRUE)

    # data tools
    observeEvent(input$newSubgroupVUI, {
      N <- input$newSubgroupVUI
      insertUI(paste0("#views"),
               ui = subgroupVUI(id = session$ns(paste0("panel", N))))
      subgroupVServer(id = paste0("panel", N),
                      cdata = view$cdata,
                      hdlist = view$hdlist)
    })
  })

}

#' Shiny app launcher for multi-view module
#'
#' @family multiVUI module functions
#'
#' @export
multiVUIR <- function(ns, ...) {
  ui <- multiVUI(ns)
  server <- multiVServer(ns, ...)
  shinyApp(ui = ui, server = server)
}
