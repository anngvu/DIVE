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
             textInput("cohortName", "Your cohort name/label (optional)", value = "", placeholder = "e.g. 'DiViD', 'pilot'.."),
             fileInput("cohortDataUpload",  HTML("<strong>Upload data to begin</strong>"), multiple = FALSE,
                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                       buttonLabel = "Data")
         )
  ),
  column(1, 
         br(),
         checkboxInput("outsideCohort", "non-nPOD cohort", value = T),
         br(),
         br(),
         actionLink("cohortDataRequirements", icon = icon("exclamation-circle"), "data requirements")
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
                                          "Other" = c(`T2D donors` = "T2D", `Autoantibody-positive donors` = "AAb")))
           )
    ),
    column(2),
    column(6,
           div(id = "npodgraph", style="margin-top: -220px; z-index: 0; height: 500px;", plotlyOutput("npodgraph")),
           div(style="position: absolute; margin-top: -200px; margin-left: -140px; top:0; left:0; z-index: 1;", 
               plotlyOutput("nPie"))
    )),
  fluidRow(
    column(1),
    column(11, style="padding-top: 20px;",
           uiOutput("matchUI")
           )
    ),
  fluidRow(style="margin-top: 20px; padding-bottom: 50px;",
           column(5, style="padding-left: 50px;",
                  uiOutput("matchResult")
           ),
           column(7,
                  uiOutput("advancedMatchResult")
           )
  )
)
