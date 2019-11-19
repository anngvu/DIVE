#' Shiny app UI for matching application
#'
#' Shiny app UI for matching application
#'
#' The application UI can be viewed as a default template of how to put together several module components,
#' including \code{\link{newDataSetInput}}, \code{\link{refSubsetInput}}, \code{\link{cohortGraphOutput}},
#' to overall enable interactive exploration and matching of two different (cohort) datasets.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param subsets Optional, a list of the named subset groups. This can be used to allow only certain subsets
#' within the data available for matching.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
matchAppUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),

    fluidRow(style="margin-top:30px; margin-bottom:50px; margin-right:100px",
             column(6, style="border-right: 1px solid lightgray;",
                    newDatasetInput(ns("CohortX"))),
             column(6, refSubsetInput(ns("ref"), name = "nPOD",
                                      label = HTML("<strong>Select type of matches to get</strong>")))
             ),
    fluidRow(style="margin-top:50px; margin-bottom:50px; margin-right:100px",
             column(1),
             column(10,
                    tabsetPanel(id = ns("tabs"),
                                tabPanel("Reference graph", HPCGraphOutput(ns("hpcg")))))
    )
  )
}

#' Shiny app server for matching and exploration of two datasets
#'
#' Shiny app server for matching and exploration of two datasets
#'
#' This server function puts together a number of modular server module components with the correct
#' logic to power the interactive capabilities of the matching application.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refdata The reference dataset.
#' @param HPCG Optional graph object representing the reference dataset.
#' @param colors Colors
#' @param datakey Passed to \code{refSubsetInput}, \code{newDataset}, \code{matchResult}.
#' @param vars Optional, variables allowed to be used for matching within \code{refdata}, organized in categories
#' displayed as separate lists (mainly for user-friendliness). If not given, all variables can be used and will be displayed in one list.
#' @param guess Optional, a function for making initial guesses of matchable variables.
#' @param subsetfeat Which variable to be used as the subset variable.
#' @param informd Link to a help file that can be called with the info module.
#' @param appdata See ?
#' @export
matchApp <- function(input, output, session,
                     refdata,
                     HPCG, colors,
                     datakey, xname, refname,
                     vars,
                     guess = DIVE::guessMatch,
                     subsets,
                     subsetfeat,
                     informd = system.file("help/cohort_exchange.Rmd", package = "DIVE"),
                     factorx,
                     appdata = NULL) {

  cohortGraph <- callModule(HPCGraph, "hpcg",
                            hpcg = HPCG,
                            colors = colors)

  newCohort <- callModule(newDataset, "CohortX",
                          datakey = datakey,
                          xname = xname,
                          checkFun = checkCohortData,
                          informd = informd,
                          appdata = appdata)

  # Optional cross-checking
  crossCheck <- callModule(intermediate, "internal",
                        data = newCohort,
                        Fun = xCheckID)

  reference <- callModule(refSubset, "ref",
                     refdata = refdata,
                     subsetfeat = subsetfeat,
                     subsets = subsets,
                     datakey = datakey,
                     refname = refname,
                     exclude = crossCheck)

  parameters <- callModule(matchLink, "params",
                           refData = reference,
                           setX = newCohort,
                           vars = vars,
                           guess = guess)

  results <-  callModule(matchResult, "results",
                         refSubset = reference,
                         setX = newCohort,
                         params = parameters,
                         sourcecol = datakey)

  explore <- callModule(exploreMore, "explore",
                        s1Data = refdata,
                        s2Data = newCohort,
                        datakey = datakey,
                        refname = refname,
                        factorx = factorx,
                        results = results)

  #-- Show output tabs ------------------------------------------------------------------------------------------#

  # Need to keep track of first upload since previous implementation of removing and adding new tabs
  # results in puzzling behavior where one must click twice on run to get results.
  # (as opposed to logic of adding tab if it's the first interaction and showing/hiding for all subsequent)
  first <- reactiveValues(upload = TRUE, result = TRUE)

  observeEvent(newCohort(), {
    if(first$upload) {
      appendTab("tabs", select = T,
                tabPanel("Match parameters", div(style="padding-top:25px", matchLinkUI(session$ns("params")))))
      first$upload <- FALSE
    } else {
      showTab("tabs", "Match parameters", select = T)
      hideTab("tabs", "Match results")
      removeTab("tabs", "Explore")
    }
    appendTab("tabs", tabPanel("Explore", div(style="padding-top:25px",
                                              exploreMoreUI(session$ns("explore"),
                                                            s1Label = "nPOD", s1Data = refdata,
                                                            s2Label = "CohortX", s2Data = newCohort,
                                                            placeholder = "(select from cohort attributes)"))))
  })

  observeEvent(results$matchtable, {
    if(first$result) {
      insertTab("tabs", select = T, target = "Match parameters", position = "after",
                tabPanel("Match results", div(style="padding-top:25px", matchResultOutput(session$ns("results")))))
      first$result <- FALSE
    } else {
      showTab("tabs", "Match results", select = T)
    }

  })

}

#' Launch Shiny app for matching between two datasets
#'
#' Wrapper to launch app at console
#'
#' @param ns Optional namespace for app, passed into server module.
#' @param ... Parameters passed into server module.
#' @export
matchAppR <- function(ns = NULL, ...) {
  ui <- matchAppUI(ns)
  server <- function(input, output, session) { callModule(matchApp, ns, ...) }
  shinyApp(ui = ui, server = server)
}
