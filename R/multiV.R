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
            # shinyWidgets::chooseSliderSkin("Flat"),
            fluidRow(class = "top-panel multiVUI-panel", id = ns("multiVUI"),
                     column(12,
                            div(class = "input-panel", span(class = "panel-label", "Data Sources"),
                                div(class = "ui-inline", multiVCtrlUI(ns("ctrl")))))
            ),
            div(class = "linked-tip", span(class = "panel-label", "Tools & extensions"),
                div(tags$a(href = "#", "read beta documentation"))),
            fluidRow(column(9, geneVUI(ns("gene"))),
                     column(3, selectVUI(ns("cdata")))
            ),
            div(id = ns("displaytrack"), class = "displaytrack")
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
#' @family multiV functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @inheritParams multiVCtrlServer
#' @inheritParams selectVServer
#' @inheritParams geneVServer
#' @export
multiVServer <- function(id,
                         hdlist = NULL,
                         choices = DIVE::hdlistchoicesMake(hdlist),
                         cdata = NULL,
                         preselect = NULL) {

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
    gselect <- geneVServer("gene")

    # "data representative" stores data forwarded by each xV and desired application for data
    datarep <- reactiveValues(localhdata = NULL, localcdata = NULL, app = NULL, event = NULL)

    # each dataset gets its own section with its own xV local module options
    # TO DO: consider non-dynamically rendered UI implementation
    obs_x <- observeEvent(view$hdata, {
        trackID <- session$ns(names(view$hdata))
        trackdata <- view$hdata[[1]]
        if(!is.null(trackdata)) {

          height <- if(nrow(trackdata) <=10) { 400 } else { 25 * nrow(trackdata) } # used across plots, tracks
          insertUI(selector = paste0("#", session$ns("displaytrack")), immediate = T,
                   ui = tags$div(id = trackID, class = "xV-container", style = paste0("min-height: ", height+30, "px ;"),
                                 xVUI(id = trackID, title = attr(trackdata, "title"))))
          xVServer(id = names(view$hdata),
                   hdata = trackdata,
                   cdata = vselect,
                   selected = gselect,
                   height = height,
                   returndata = datarep)
          showNotification(ui = "appended new dataset view", duration = 2)
        } else {
          removeUI(selector = paste0("#", trackID))
          showNotification(ui = "removed dataset from view", duration = 2)
        }
    }, ignoreInit = TRUE)

    # Data tools --------------------------------------------------------------------#

    observeEvent(datarep$event, {
      if(datarep$app == "contrast") {
        showModal(
          modalDialog(
            contrastVUI(session$ns("new-contrast"), choices = names(datarep$localcdata)),
            easyClose = FALSE, footer = NULL))
      }
    })

    contrastVServer("new-contrast",
                    cdata = reactive(datarep$localcdata),
                    selected_dataset = reactive(datarep$localhdata))

  })

}

#' Shiny app launcher for multi-view module
#'
#' @family multiV module functions
#'
#' @param ns Namespace of app module.
#' @param ... Arguments passed to \code{\link{multiVServer}}.
#' @export
multiVUIR <- function(ns, ...) {
  ui <- multiVUI(ns)
  server <- multiVServer(ns, ...)
  shinyApp(ui = ui, server = server)
}
