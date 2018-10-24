fluidPage(fluidRow(
  column(1,
         br(),
         actionButton("guideMatch", "Guide", icon = icon("info-circle"))
  ),
  column(1, 
         h3("CohortX"),
         helpText("(your cohort)")
  ),
  column(2,
         div(id = "cohortInput", 
             textInput("cohortname", "Your cohort name/label (optional)", value = "", placeholder = "e.g. 'DiViD', 'pilot'.."),
             fileInput("cohortDataUpload",  HTML("<strong>Upload data to begin</strong>"), multiple = FALSE,
                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                       buttonLabel = "Data")
         )
  ),
  column(1, 
         br(),
         checkboxInput("outsideCohort", "non-nPOD cohort", value = T),
         br(),
         helpText("How does this work?"),
         actionLink("cohordatauploadhelp", "See details and methodology.")
  ),
  column(1
  ),
  column(6
  )),
  fluidRow(
    column(1
    ),
    column(1, style="border-top: 1px solid lightgray;",
           br(),
           h3("nPOD")
    ),
    column(2, style="border-top: 1px solid lightgray;",
           br(),
           div(id = "nPODInput", 
               selectInput("matchType", "Type of matches to get from nPOD", 
                           choices = list("No diabetes (negative control)" = c(`No-diabetes donors` = "ND"),
                                          "T1D (positive control)" = c(`T1D donors` = "T1D"),
                                          "Other" = c(`T2D donors` = "T2D", `Autoantibody-positive donors` = "AAb"))),
               div(style="padding-bottom: 5px;", HTML("<strong>You are matching on:</strong>")),
               verbatimTextOutput("matchOn", placeholder = TRUE),
               actionButton("match", "Match")
           )
    ),
    column(2, style="padding-top: 10px;",
           br(),
           uiOutput("matchUIhelp")
    ),
    column(6,
           div(id = "npodgraph", style="margin-top: -220px; z-index: 0; height: 500px;", plotlyOutput("npodgraph")),
           div(style="position: absolute; margin-top: -200px; margin-left: -140px; top:0; left:0; z-index: 1;", 
               plotlyOutput("nPie"))
    )),
  fluidRow(
    column(1),
    column(6, style="padding-top: 20px;",
           fluidRow(div(id = "matchUI-container",
                        column(4,
                               uiOutput("matchCovariatesX")
                        ),
                        column(4,
                               uiOutput("matchCovariatesD")
                        ),
                        column(4,
                               uiOutput("matchCovariatesC")
                        ))
           )),
    column(5,
           br()
    )),
  fluidRow(style="margin-top: 30px; padding-bottom: 50px;",
           column(1),
           column(3,
                  uiOutput("matchResult")
           ),
           column(1),
           column(3,
                  uiOutput("advancedMatchresult")
           ),
           column(4,
                  uiOutput("advancedMatchResult2")
           )       
  )
)
