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
#' @param SUBSETFEAT The column containing the grouping feature in REFDATA.
#' @param SUBSETS Optional, grouped subsets within REFDATA. This can be used to allow only certain subsets
#' within the data available for matching.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
matchApp <- function(REFDATA = npodX, NGRAPH = npodgraph, NGRAPHDATA = ndata,
                     REFKEY = list(Cohort = "nPOD", Cohort = "CohortX"),
                     VARS = list(
                       Clinical = c("BMI", "db.duration", "age.onset",
                                    "Cpeptide", "HbA1c", "peak.gluc",
                                    "GADA.pos", "IA2A.pos", "mIAA.pos", "ZnT8A.pos", "AutoAb.count"),
                       Demographic = c("age", "sex_Female", "sex_Male",
                                       "race_Caucasian", "race_AfricanAmerican", "race_Hispanic.Latino",
                                       "race_Asian", "race_AmericanIndian", "race_Multiracial")
                     ),
                     SUBSETFEAT = "donor.type",
                     SUBSETS = list("No diabetes" = list("No diabetes", "Other-No Diabetes"),
                                    "Autoantibody positive" = list("Autoab Pos"),
                                    "Type 1 Diabetes" = list("T1D", "T1D Medalist", "Monogenic Diabetes"),
                                    "Other diabetes" = list("T2D", "Gestational diabetes"),
                                    "Other" = list("Transplant", "Cystic fibrosis", "Pregnancy", "Gastric Bypass")),
                     CSS = system.file("Apps/www/", "app.css", package = "DIVE")) {
  ui <- fluidPage(theme = shinytheme("lumen"), includeCSS(CSS),

                  fluidRow(style="margin-top:50px; margin-bottom:50px; margin-right:100px",
                           column(6,  style="border-right: 1px solid lightgray;",
                                  newDatasetInput("CohortX", type = "cohort", hasInfo = T)),
                           column(6,
                                  refSubsetInput("nPOD", label = HTML("<strong>Select type of matches to get</strong>"),
                                                 subsets = SUBSETS)
                           )),
                  fluidRow(style="margin-top:50px; margin-bottom:50px; margin-right:100px",
                           column(1),
                           column(10,
                                  tabsetPanel(id = "tabs",
                                              tabPanel("Reference cohort graph", cohortGraphOutput("nPODg")))
                           ))
  )

  server <- function(input, output, session) {

    cohortGraph <- callModule(cohortGraph, "nPODg",
                              cohgraph = NGRAPH,
                              cohdata = NGRAPHDATA)

    nPOD <- callModule(refSubset, "nPOD",
                       refData = REFDATA,
                       subsetfeat = SUBSETFEAT,
                       refkey = REFKEY[1])

    newCohort <- callModule(newDataset, "CohortX",
                            refkey = REFKEY[2],
                            infoRmd = "Help/match.Rmd")

    parameters <- callModule(matchLink, "params",
                             refData = nPOD,
                             cohortX = newCohort,
                             vars = VARS,
                             guess = guessMatch)

    results <-  callModule(matchResult, "match",
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
      appendTab("tabs", tabPanel("Match parameters", div(style="padding-top:25px", matchLinkUI("params"))), select = T)
      appendTab("tabs", tabPanel("Explore",
                                 div(style="padding-top:25px",
                                     exploreMoreUI("explore",
                                                   s1Label = "nPOD", s1Data = REFDATA,
                                                   s2Label = "CohortX", s2Data = newCohort,
                                                   placeholder = "(select from cohort attributes)"))), select = F)
    })

    observeEvent(results$matchtable, {
      removeTab("tabs", "Match results")
      insertTab("tabs", tabPanel("Match results", div(style="padding-top:25px", matchResultOutput("match"))),
                target = "Match parameters", position = "after", select = T)
    })

  }

  shinyApp(ui = ui, server = server)

}
