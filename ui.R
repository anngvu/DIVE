library(shinythemes)
library(shinyjqui)
library(shinycssloaders)

shinyUI(
  navbarPage("nPOD DIVE", id = "main", selected = "intro", 
             theme = shinytheme("lumen"), tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
             
             #-- PAGE 1 ----------------------------------------------------------------------------------------#                 
             tabPanel("Connections in Investigations", value = "intro", # icon = icon("connectdevelop"),
                      fluidRow(
                        column(8,
                               visNetworkOutput("network", height = "750px"),
                               actionButton("accessdata", "Data accessible", icon = icon("circle", class = "node")),
                               actionButton("noaccessdata", "Data not accessible", icon = icon("circle", class = "node"))
                        ),
                        column(4,
                               includeMarkdown("Intro.Rmd")
                        )
                      )),
             #-- PAGE 2 ----------------------------------------------------------------------------------------#
             navbarMenu("Integrative Data Views",
             
             tabPanel("Cohort exploration and sharing", value = "data-fusion-1", # icon = icon("cube"), 
                      fluidPage(fluidRow(
                        column(1,
                               br(),
                               actionButton("helpFusion", "", icon = icon("info-circle"))
                        ),
                        column(1, 
                               h3("CohortX"),
                               helpText("(your cohort)")
                        ),
                        column(2,
                               textInput("cohortname", "Your cohort name/label (optional)", value = "", placeholder = "e.g. 'DiViD', 'TEDDY', 'pilot'.."),
                               fileInput("cohortDataUpload",  HTML("<strong>Upload data to begin</strong>"), multiple = FALSE,
                                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                                         buttonLabel = "Data")
                        ),
                        column(2, 
                               br(),
                               br(),
                               br(),
                               br(),
                               helpText("What should be included?"),
                               actionLink("cohordatauploadhelp", "See guide.")
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
                                 selectInput("matchType", "Type of matches to get from nPOD", 
                                             choices = list("No diabetes (negative control)" = c(`No-diabetes donors` = "ND"),
                                                            "T1D (positive control)" = c(`T1D donors` = "T1D"),
                                                            "Other" = c(`T2D donors` = "T2D", `Autoantibody-positive donors` = "AAb"))),
                                 div(style="padding-bottom: 5px;", HTML("<strong>You are matching on:</strong>")),
                                 verbatimTextOutput("matchOn", placeholder = TRUE),
                                 actionButton("match", "Match")
                          ),
                          column(2, style="padding-top: 10px;",
                                helpText("Refer to the right for a visual summary of nPOD's data availability. 
                                         Covariates with more missing data work less well for matching."),
                                br(),
                                uiOutput("matchUIhelp")
                          ),
                          column(6, style="margin-top: -220px",
                                 plotlyOutput("npodgraph")
                                 )
                          ),
                        fluidRow(
                          column(1),
                          column(6, style="padding-top: 20px;",
                                 fluidRow(
                                          column(4,
                                                 uiOutput("matchCovariatesX")
                                          ),
                                          column(4,
                                                 uiOutput("matchCovariatesD")
                                          ),
                                          column(4,
                                                 uiOutput("matchCovariatesC")
                                          )
                                 )),
                          column(5, #style="background-color: lightgray", 
                                 br()
                          )),
                      fluidRow(
                        column(1),
                        column(1 #, style="border-top: 1px solid lightgray;"
                        ),
                        column(4, # style="border-top: 1px solid lightgray; padding-top: 20px;",
                               uiOutput("matchResult")
                        ),
                        column(2, 
                               tableOutput("otherAttributes")
                        ),
                        column(4,
                               ""
                        )       
                    ))
             )),
             
             
             #-- PAGE 3 ----------------------------------------------------------------------------------------#
             tabPanel("Experimental data", value = "data-fusion-2",
                      fluidPage(fluidRow(
                        column(1,
                               br(),
                               actionButton("helpCorrelation", "", icon = icon("info-circle"))
                        ),
                        column(5,
                               div(class = "forceInline", numericInput("minimumN", HTML("min. N for <i>r</i>"), min = 2, max = NA, step = 1, val = 5, width = "80px")),
                               HTML("&nbsp"),
                               div(class = "forceInline", selectInput("varMenuOpt", "Filter variables by", 
                                                                      choices = c(`variable name` = "variable", `variable category` = "category", `parent source` = "author"), 
                                                                      width = "150px")),
                               div(class = "forceInline", selectizeInput("varMenu", "Exclude/keep in correlation matrix:", colnames(cor.data$corM), multiple = T, 
                                                                         options= list(placeholder = "select..."), width = "300px")),
                               div(class = "forceInline", br(), actionButton("varExclude", "Exclude")),
                               div(class = "forceInline", br(), actionButton("varKeep", "Keep")),
                               div(class = "forceInline", br(), actionButton("varReset", "Reset", icon = icon("undo"))) 
                        ),
                        column(2, 
                               div(class = "forceInline", fileInput("dataUpload", "", multiple = FALSE, width = "250px", 
                                                                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                                                                    buttonLabel = "My data", placeholder = "Upload to compare..")),
                               div(class = "forceInline", br(), actionButton("helpUpload", "", icon = icon("question-circle"), width = "10px"))
                        ),
                        column(4, 
                               selectizeInput("drilldown", "Drill down to data points for V1 or V1 x V2:", 
                                              choices = c("", colnames(cor.data$corM)), selected = "", options = list(maxItems = 2), width = "300px")
                        )),
                        fluidRow(
                          column(8, align = "left",
                                 plotlyOutput("corM")
                          ),
                          column(4,
                                  conditionalPanel("input.drilldown",
                                                   div(class = "forceInline", 
                                                       selectInput("colorby", "Color data points by", 
                                                                   choices = names(cdata)[!names(cdata) %in% "ID"], 
                                                                   selected = "donor.type", width = "200px")),
                                                   HTML("&nbsp"),
                                                   div(class = "forceInline", br(), actionButton("switchXY", "XY", icon = icon("refresh"))),
                                                   HTML("&nbsp"),
                                                   div(class = "forceInline", br(), checkboxInput("plotsmooth", "Add smooth"))),
                                 plotlyOutput("scatter")
                          ))
                      )),
             #-- PAGE 4 ----------------------------------------------------------------------------------------#
             tabPanel("Experimental data (high-throughput)", value = "HD", # icon = icon("cubes"),
                      fluidRow(class = "top-options",
                               column(1,
                                      br(),
                                      actionButton("helpVolcano", "", icon = icon("info-circle")),
                                      actionButton("viewVolcano", "", icon = icon("eye-slash")),
                                      div(class = "forceInline", br(), actionButton("resetVolcano", "Reset", icon = icon("undo")))
                               ),       
                               column(3,
                                      div(class = "forceInline", selectizeInput("Glist", "Genes (proteins) of interest", 
                                                                                choices = NULL, selected = NULL, 
                                                                                options = list(maxItems = 50)), width = "375px"),
                                      div(class = "forceInline", br(), actionButton("highlight", "", icon = icon("check"))),
                                      div(class = "forceInline", br(), actionButton("gSets", "", icon = icon("list"))),
                                      helpText("Note: Expression values may not be available in all datasets.")
                               ),
                               column(2, align="right", 
                                      radioButtons("GorR", "Look up genes (proteins) involved in a process/pathway with:", 
                                                   choices = c("Gene Ontology", "Reactome"),
                                                   selected = "Gene Ontology", inline = T),
                                      conditionalPanel(condition = "input.GorR == 'Gene Ontology'",
                                                       radioButtons("BPCCMP", "GO branch",
                                                                    choices = c("BP" = "BP", "CC" = "CC", "MF" = "MF"),
                                                                    selected = "BP", inline = T))
                               ),
                               column(3,
                                      div(class = "forceInline", selectizeInput("GOReactq", "Choose term", 
                                                                                choices = NULL, selected = NULL, width = "240px")),
                                      div(class = "forceInline", br(), actionButton("Ont", "", icon = icon("check"))),
                                      helpText("Only terms with at least one annotation shown.")
                               ),
                               column(3, style="border-left: 1px solid lightgray",
                                      div(class = "forceInline", selectizeInput("Clist", "Phenotype/clinical variable(s)", 
                                                                                choices = Columns[Source %in% c("Aab", "Demographics", "DiabetesInfo", "HLARisk"), Variable], 
                                                                                selected = character(0), multiple = T, 
                                                                                options = list(placeholder = "select genes first", maxItems = 3), width = "240px")),
                                      div(class = "forceInline", br(), actionButton("clistAdd", "", icon = icon("check"))),
                                      helpText("See gene/protein expression values relative to phenotype/clinical data.")
                               )),
                      fluidRow(
                        column(12,
                               conditionalPanel("input.viewVolcano%2==1",
                                                checkboxGroupInput("activeVolcano", "Show/hide volcano plots:", 
                                                                   choiceNames = c("Transcriptomics | AAB-HC (Yip et et. unpublished)", 
                                                                                   "Transcriptomics | T1D-HC (Yip et et. unpublished)", 
                                                                                   "Proteomics | T1D-HC (Liu et al. 2016)", 
                                                                                   "Proteomics | T1D-HC (Nyalwidhe et al. 2017)", 
                                                                                   "Proteomics | AAB-HC (Nyawidle et al. 2016)"),
                                                                   choiceValues = c("gx.AAB", "gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                                                   selected = c("gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                                                   inline = T)
                               )
                        )),
                      fluidRow(
                        column(12,
                               withSpinner(plotlyOutput("volcanos"), type = 4, color = "gray")
                        )),
                      fluidRow(
                        column(12, style="margin-top: 50px;",
                               radioButtons("whichX", "Dataset", 
                                            choices = c("Transcriptomics | Pancreas" = "gx", "Proteomics | Exocrine" = "px1", "Proteomics | Endocrine" = "px2"), 
                                            selected = "gx", inline = T)
                        ),
                      column(12,
                               plotlyOutput("parallel")
                      )
             )
             )
             ),
             #-- PAGE 5 ----------------------------------------------------------------------------------------#
             tabPanel("Vignettes", value = "stories" #icon = icon("asterisk")
                      
             ),
             
             #-- PAGE 6 ----------------------------------------------------------------------------------------#
             tabPanel("Get Data", value = "source-data", # icon = icon("database"),
                      checkboxInput("filterDT", "Only display sources where individual-level data is readily available.", value = T, width = 500),
                      helpText("'Get from original source' link points to the original data in a supplemental file 
                               or to an external database where data has been deposited. The original sources can provide more detail about
                               methodology, definitions and other metadata, but are in a variety of formats not universally machine-readable (e.g. PDF, Excel). 
                               To facililate re-use, curated data can also be downloaded all at once (except for some high-throughput datasets) 
                               as a collection of plain text tab-delimited  files."),
                      downloadButton("downloadCollection", label = "Download Collection"),
                      DT::dataTableOutput("sourceDT")
             ),
             
             #-- PAGE 7 ----------------------------------------------------------------------------------------#
             tabPanel("Give Data", value = "give", # icon = icon("database"),
                     ""
             )
))
