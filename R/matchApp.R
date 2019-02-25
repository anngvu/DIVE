#' Launch Shiny app for matching and exploration of two datasets
#'
#' The original use case is for cohort matching.
#'
#' @param REFDATA A data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param NGRAPH A matrix of the same dimensions as M with data for the filterable layer, e.g. sample size.
#' @param NGRAPHDATA The non-reactive data used for generating the matrix.
#' @param VARS 
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
matchApp <- function(REFDATA = npodX, NGRAPH = npodgraph, NGRAPHDATA = ndata, 
                     VARS = list(
                       Clinical = c("BMI", "db.duration", "age.onset",
                                    "Cpeptide", "HbA1c", "peak.gluc",
                                    "GADA.pos", "IA2A.pos", "mIAA.pos", "ZnT8A.pos", "AutoAb.count"),
                       Demographic = c("age", "sex_Female", "sex_Male",
                                       "race_Caucasian", "race_AfricanAmerican", "race_Hispanic.Latino",
                                       "race_Asian", "race_AmericanIndian", "race_Multiracial")
                     ),
                     CSS = system.file("App/www/", "app.css", package = "DIVE")) {
  ui <- fluidPage(theme = shinytheme("lumen"), includeCSS(CSS),
                  
                  fluidRow(style="margin-top:50px; margin-bottom:50px; margin-right:100px",
                           column(6,  style="border-right: 1px solid lightgray;",
                                  newDatasetInput("CohortX", type = "cohort", hasInfo = T)),
                           column(6,
                                  refSubsetInput("nPOD", label = HTML("<strong>Select type of matches to get from nPOD</strong>"),
                                                 subsets = list("No diabetes" = c(`No diabetes` = "ND"),
                                                                "T1D" = c(`T1D and T1D Medalist` = "T1D"),
                                                                "Other" = c(`T2D` = "T2D", `Autoantibody-positive` = "AAb")))
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
    
    newCohort <- callModule(newDataset, "CohortX", 
                            refkey = list(Cohort = "CohortX"), 
                            infoRmd = "Help/match.Rmd")
    
    nPOD <- callModule(refSubset, "nPOD", 
                       refData = REFDATA, 
                       subsetfeat = "donor.type",  
                       subsetlist = list(ND = "No diabetes", T1D = list("T1D", "T1D Medalist"), T2D = "T2D", AAb = "Autoab Pos"),
                       refkey = list(Cohort = "nPOD"))
    
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
