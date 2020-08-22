#' Shiny app UI for matching application
#'
#' Shiny app UI for matching application
#'
#' The application UI can be viewed as a default template of how to put together several module components,
#' including \code{\link{newDataSetInput}}, \code{\link{refSubsetInput}}, \code{\link{cohortGraphOutput}},
#' to overall enable interactive exploration and matching of two different datasets. Note that though the datasets
#' are typically conceived as datasets of matchable patient cohorts, this could be other types of matchable data
#' such as geographic sampling sites, etc.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to set look and feel of the module.
#' @param addonUI Optional, ui module function to display addon components.
#' @param addontab If addonUI is given, should also give a name for the tab hosting addon ui components.
#' @export
matchAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE"),
                       refname = NULL, addonUI = NULL, addontab = "") {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),

    fluidRow(style="margin-top:30px; margin-bottom:50px; margin-right:100px",
             column(6, style="border-right: 1px solid lightgray;",
                    newDatasetInput(ns("CohortX"))),
             column(6, refSubsetInput(ns("ref"), refname = refname))
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
#' This server function puts together a number of modular components to
#' implement the whole interaction of the matching application. Though originally composed for
#' matching patient cohorts, this matching application can be used with data in different domains.
#' The composition of this module allows inserting a addon-written module.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refdata Data.table of reference dataset.
#' @param datakey Shared grouping parameter passed to \code{refSubset}, \code{newDataset}, \code{matchResult}.
#' @param xname Character label for the external dataset, e.g. "CohortX" or "New Site" or "Comparison Dataset".
#' @param refname Character label for the reference dataset.
#' @param vars Variables allowed to be used for matching within \code{refdata}, which will be organized and
#' displayed as the given named lists.
#' @param guess Optional, a function for making initial guesses of matchable variables.
#' @param subsets The subsets.
#' @param subsetfeat Which variable to be used as the subset variable.
#' @param factorx A naming pattern for factoring variables displayed.
#' @param informd Optional, link to a help file that can be called with the info module.
#' @param addon Optional, a self-contained module that doesn't interact with others,
#' e.g. for extra summary plots. Needs to be passed in as name of the server module function.
#' @param appdata Optional, a path to example application data (for demonstration purposes).
#' @export
matchAppServer <- function(id,
                           refdata,
                           datakey, xname, refname,
                           vars, guess = NULL,
                           subsets, subsetfeat,
                           factorx,
                           informd = system.file("help/cohort_exchange.Rmd", package = "DIVE"),
                           addon = NULL, appdata = NULL) {

  moduleServer(id, function(input, output, session) {

    if(!is.null(addon)) callModule(addon, "addon")

    newCohort <- newDatasetServer("CohortX",
                                  datakey = datakey,
                                  xname = xname,
                                  checkFun = checkCohortData,
                                  informd = informd,
                                  appdata = appdata)

    # Optional cross-checking
    crossCheck <- callModule(intermediate, "internal",
                             data = newCohort,
                             Fun = xCheckID)

    reference <- refSubsetServer("ref",
                                 refdata = refdata,
                                 subsetfeat = subsetfeat,
                                 subsets = subsets,
                                 datakey = datakey,
                                 refname = refname,
                                 exclude = crossCheck)

    parameters <- matchLinkServer("params",
                                  refdata = reference,
                                  setX = newCohort,
                                  vars = vars,
                                  guess = guess)

    results <-  matchResultServer("results",
                                  refSubset = reference,
                                  setX = newCohort,
                                  params = parameters,
                                  sourcecol = datakey)

    explore <- matchPlotServer("explore",
                               s1Data = refdata,
                               s2Data = newCohort,
                               datakey = datakey,
                               refname = refname,
                               factorx = factorx,
                               results = results)

    #-- Output tabs display logic ------------------------------------------------------------------------------------------#

    # Need to keep track of first upload since previous implementation of removing and adding new tabs
    # results in puzzling behavior where one must click twice on run to get results.
    # (vs. currently adding tab if it's the first interaction and showing/hiding for all subsequent)
    userfirst <- reactiveValues(upload = TRUE, result = TRUE)

    observeEvent(newCohort(), {
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
                                     s1Label = refname, s1Data = refdata,
                                     s2Label = xname, s2Data = newCohort,
                                     placeholder = "(select from attributes)")))
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
#' @param ... Parameters passed into server modules.
#' @export
matchApp <- function(ns = NULL, ...) {
  ui <- matchAppUI(ns)
  server <- matchAppServer(ns, ...)
  shinyApp(ui = ui, server = server)
}
