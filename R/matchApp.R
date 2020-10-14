#' Shiny app UI for matching application
#'
#' Shiny app UI for matching application
#'
#' The application UI puts together multiple module components to enable
#' exploration and matching of two different but same-type datasets.
#' It places a \code{customDatasetInput} component for a user-uploaded input dataset in the top left
#' and a \code{dataSubsetInput} component to allow selection of the match pool in the top right.
#' The bottom half is a tab panel with tabs dynamically shown for a matching interface,
#' result table and result plots.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to set look and feel of the module.
#' @param refname Optional, name of the reference dataset.
#' @param addonUI Optional, ui module function to display addon components.
#' @param addontab If addonUI is given, should also give a name for the tab hosting addon ui components.
#' @import shiny
#' @export
matchAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE"),
                       refname = NULL, addonUI = NULL, addontab = "") {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),

    fluidRow(class = "matchAppUI-panel",
             column(1),
             column(4, div(class = "card-panel", div(class = "panel-header", "External set"), customDatasetInput(ns("CohortX")))),
             column(4, div(class = "card-panel", div(class = "panel-header", "Reference set"), dataSubsetInput(ns("ref"), label = refname)))
             ),
    fluidRow(style="margin-top:50px; margin-bottom:50px; margin-right:100px",
             column(1),
             column(10,
                    if(is.null(addonUI)) { tabsetPanel(id = ns("tabs")) }
                    else { tabsetPanel(id = ns("tabs"), tabPanel(addontab, addonUI(ns("addon")))) }
                    )
            )
    )
}

#' Shiny app server for matching and exploration of two datasets
#'
#' Shiny app server for matching and exploration of two datasets
#'
#' This server function puts together modular components to
#' implement the whole interaction of the matching application with several defaults
#' to facilitate quick startup.
#' Though originally conceived for matching and exploration of patient cohort datasets,
#' this could be adapted to matchable data in other domains, such as geographic sampling sites, etc.
#' This app module can also be viewed as a starting template to help
#' compose a new matching app with a sufficiently different layout or functionality.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param refname Name of the reference dataset, used for labels. Defaults to "Reference Cohort".
#' @param refdata Reference dataset that can be matched against in whole or in part, i.e. the match pool.
#' @param customdata A name for the appended colum to user-uploaded input data.
#' This depends on the type of data expected. "Cohort" is the provided default for cohort-matching application.
#' See \code{\link{customDatasetServer}}.
#' @param defaultvalue Default attribute value to append to user-uploaded input data.
#' "CohortX" is the provided default for cohort-matching application. See \code{\link{customDatasetServer}}.
#' @inheritParams dataSubsetServer
#' @inheritParams dataUploadServer
#' @inheritParams matchLinkServer
#' @inheritParams matchResultServer
#' @inheritParams matchPlotServer
#' @import shiny
#' @export
matchAppServer <- function(id,
                           refname = "Reference Cohort",
                           refdata,
                           subsetv, subsets,
                           customdata = "Cohort",
                           defaultvalue = "CohortX",
                           vars, guess = NULL,
                           informd = system.file("info/cohort_exchange.Rmd", package = "DIVE"),
                           appdata = NULL) {

  moduleServer(id, function(input, output, session) {

    reference <- dataSubsetServer(id = "ref",
                                  dataset = refdata,
                                  subsetv = subsetv,
                                  subsets = subsets)

    newcohort <- customDatasetServer(id = "CohortX",
                                     customdata = customdata,
                                     defaultvalue = defaultvalue,
                                     checkFun = checkCohortData,
                                     informd = informd,
                                     appdata = appdata)

    # Cross-checking for internal matching where newcohort includes ids in reference
    refcohort <- reactive({ excludePresent(data1 = newcohort(), data2 = reference()) })


    parameters <- matchLinkServer(id = "params",
                                  refdata = refcohort,
                                  inputdata = newcohort,
                                  vars = vars,
                                  guess = guess)

    results <-  matchResultServer(id = "results",
                                  refdata = refcohort,
                                  inputdata = newcohort,
                                  params = parameters,
                                  sourcecol = "Cohort")

    explore <- matchPlotServer(id = "explore",
                               s1data = refcohort,
                               s2data = newcohort,
                               results = results)

    #-- Output tabs display logic ------------------------------------------------------------------------------------------#

    # Need to keep track of first upload since previous implementation of removing and adding new tabs
    # results in puzzling behavior where one must click twice on run to get results.
    # (vs. currently adding tab if it's the first interaction and showing/hiding for all subsequent)
    userfirst <- reactiveValues(upload = TRUE, result = TRUE)

    observeEvent(newcohort(), {
      if(userfirst$upload) {
        appendTab("tabs", select = T,
                  tabPanel("Match parameters", matchLinkUI(session$ns("params"))))
        userfirst$upload <- FALSE
      } else {
        showTab("tabs", "Match parameters", select = T)
        hideTab("tabs", "Match results")
        removeTab("tabs", "Explore")
      }
      appendTab("tabs",
                tabPanel("Explore",
                         matchPlotUI(session$ns("explore"),
                                     s1label = refname, s2label = defaultvalue)))
    })

    observeEvent(results$matchtable, {
      if(userfirst$result) {
        insertTab("tabs", select = T, target = "Match parameters", position = "after",
                  tabPanel("Match results", matchResultOutput(session$ns("results"))))
        userfirst$result <- FALSE
      } else {
        showTab("tabs", "Match results", select = T)
      }
    })

  })
}

#' Launch Shiny app for matching between two datasets
#'
#' Wrapper to launch app at console
#'
#' @param ns Optional namespace for app, passed into server module.
#' @param ... Parameters passed into server module.
#' @export
matchApp <- function(ns = NULL, ...) {
  ui <- matchAppUI(ns)
  server <- matchAppServer(ns, ...)
  shinyApp(ui = ui, server = server)
}
