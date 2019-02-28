#' Launch Shiny app for matching and exploration of two datasets
#'
#' The original use case is for cohort matching.
#'
#' @param id
#' @param SUBSETS Optional, grouped subsets within REFDATA. This can be used to allow only certain subsets
#' within the data available for matching.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
matchAppUI <- function(id,
                       SUBSETS = list("No diabetes" = list("No diabetes", "Other-No Diabetes"),
                                    "Autoantibody positive" = list("Autoab Pos"),
                                    "Type 1 Diabetes" = list("T1D", "T1D Medalist", "Monogenic Diabetes"),
                                    "Other diabetes" = list("T2D", "Gestational diabetes"),
                                    "Other" = list("Transplant", "Cystic fibrosis", "Pregnancy", "Gastric Bypass")),
                       CSS = system.file("www/", "app.css", package = "DIVE"))
{
  ns <- NS(id)
  fluidPage(theme = shinytheme("lumen"), includeCSS(CSS),

            fluidRow(style="margin-top:30px; margin-bottom:50px; margin-right:100px",
                     column(6, style="border-right: 1px solid lightgray;",
                            newDatasetInput(ns("CohortX"), "CohortX", type = "cohort", hasInfo = T)),
                     column(6,
                            refSubsetInput(ns("nPOD"), "nPOD", label = HTML("<strong>Select type of matches to get</strong>"),
                                           subsets = SUBSETS)
                     )),
            fluidRow(style="margin-top:50px; margin-bottom:50px; margin-right:100px",
                     column(1),
                     column(10,
                            tabsetPanel(id = ns("tabs"),
                                        tabPanel("Reference cohort graph",
                                                 cohortGraphOutput(ns("nPODg")))
                                        ))
                     )
  )
}

#' Launch Shiny app for matching and exploration of two datasets
#'
#' The original use case is for cohort matching.
#'
#' @param REFDATA A data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param REFKEY Passed to refSubsetInput.
#' @param NGRAPH A matrix of the same dimensions as M with data for the filterable layer, e.g. sample size.
#' @param NGRAPHDATA The non-reactive data used for generating the matrix.
#' @param VARS Optional, variables allowed to be used for matching within REFDATA organized in categories.
#' If not given, all variables in REFDATA are used without a category.
#' @export
matchApp <- function(input, output, session,
                     REFDATA = npodX, NGRAPH = npodgraph, NGRAPHDATA = ndata,
                     REFKEY = list(Cohort = "nPOD", Cohort = "CohortX"),
                     VARS = list(
                       Clinical = c("BMI", "db.duration", "age.onset",
                                    "Cpeptide", "HbA1c", "peak.gluc",
                                    "GADA.pos", "IA2A.pos", "mIAA.pos", "ZnT8A.pos", "AutoAb.count"),
                       Demographic = c("age", "sex_Female", "sex_Male",
                                       "race_Caucasian", "race_AfricanAmerican", "race_Hispanic.Latino",
                                       "race_Asian", "race_AmericanIndian", "race_Multiracial")
                     ),
                     GUESS = guessMatch,
                     SUBSETFEAT = "donor.type",
                     SAVED = "examplecohort2020") {

    cohortGraph <- callModule(cohortGraph, "nPODg",
                              cohgraph = NGRAPH,
                              cohdata = NGRAPHDATA)

    nPOD <- callModule(refSubset, "nPOD",
                       refData = REFDATA,
                       subsetfeat = SUBSETFEAT,
                       refkey = REFKEY[1])

    newCohort <- callModule(newDataset, "CohortX",
                            refkey = REFKEY[2],
                            infoRmd = "help/match.Rmd",
                            saved = SAVED)

    parameters <- callModule(matchLink, "params",
                             refData = nPOD,
                             cohortX = newCohort,
                             vars = VARS,
                             guess = GUESS)

    results <-  callModule(matchResult, "results",
                           refSubset = nPOD,
                           cohortX = newCohort,
                           params = parameters)

    explore <- callModule(exploreMore, "explore",
                          s1Data = REFDATA,
                          s2Data = newCohort,
                          results = results)

    #-- Show outputs in new tabs -------------------------------------------------------------------------------------------#
    observeEvent(newCohort(), {
      removeTab("tabs", "Match parameters")
      removeTab("tabs", "Match results")
      removeTab("tabs", "Explore")
      appendTab("tabs", select = T,
                tabPanel("Match parameters",
                         div(style="padding-top:25px",
                             matchLinkUI(session$ns("params")))))
      appendTab("tabs",
                tabPanel("Explore",
                         div(style="padding-top:25px",
                            exploreMoreUI(session$ns("explore"),
                                          s1Label = "nPOD", s1Data = REFDATA,
                                          s2Label = "CohortX", s2Data = newCohort,
                                           placeholder = "(select from cohort attributes)"))))
    })

    observeEvent(results$matchtable, {
      removeTab("tabs", "Match results")
      insertTab("tabs", target = "Match parameters", position = "after", select = T,
                tabPanel("Match results", div(style="padding-top:25px", matchResultOutput(session$ns("results")))))
    })
}

#' Launch Shiny app for exploration of relationships in annotated data with an interactive matrix
#'
#' Wrapper to launch app at console
#'
#' @export
matchAppRun <- function() {
  ui <- matchAppUI("default")
  server <- function(input, output, session) {
    callModule(matchApp, "default")
  }
 shinyApp(ui = ui, server = server)
}
