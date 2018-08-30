library(shinythemes)
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
             tabPanel("2D", value = "page-2", # icon = icon("cube"), 
                      fluidPage(fluidRow(
                        column(1,
                               br(),
                               actionButton("helpCorrelation", "", icon = icon("info-circle"))
                        ),
                        column(5,
                               div(class = "forceInline", numericInput("minimumN", HTML("min. N for <i>r</i>"), min = 2, max = NA, step = 1, val = 5, width = "80px")),
                               HTML("&nbsp"),
                               div(class = "forceInline", selectInput("varMenuOpt", "Filter variables by", choices = c("Variable name", "Variable category", "Parent publication"), width = "150px")),
                               div(class = "forceInline", selectizeInput("varMenu", "Exclude/keep in correlation matrix:", colnames(corM), multiple = T, width = "300px")),
                               div(class = "forceInline", br(), actionButton("varExclude", "Exclude")),
                               div(class = "forceInline", br(), actionButton("varKeep", "Keep")),
                               div(class = "forceInline", br(), actionButton("varReset", "Reset", icon = icon("undo"))) 
                        ),
                        column(2, 
                               div(class = "forceInline", fileInput("dataUpload", "", multiple = FALSE, width = "250px", 
                                                                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                                                                    buttonLabel = "My data..", placeholder = "upload comparison data")),
                               div(class = "forceInline", br(), actionButton("helpUpload", "", icon = icon("question-circle"), width = "10px"))
                        ),
                        column(2, 
                               selectizeInput("drilldown", "Drill down to data points for V1 or V1 x V2:", 
                                              choices = c("", colnames(corM)), selected = "", options = list(maxItems = 2))
                        ),
                        column(2, 
                               conditionalPanel("input.drilldown",
                                                selectInput("colorby", "Color data points by", 
                                                            choices = names(cdata)[!names(cdata) %in% "ID"], 
                                                            selected = "donor.type", width = "200px"),
                                                checkboxInput("plotsmooth", "Add smooth"))
                        ),
                        fluidRow(
                          column(8, align = "center",
                                 plotlyOutput("corM")
                          ),
                          column(4, align = "center",
                                 plotlyOutput("scatter")
                          ))
                      ))),
             
             #-- PAGE 3 ----------------------------------------------------------------------------------------#
             tabPanel("HD", value = "page-3", # icon = icon("cubes"),
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
                                                                                options = list(maxItems = 20))),
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
                                      div(class = "forceInline", br(), actionButton("ontologySets", "", icon = icon("list"))),
                                      helpText("Only terms with at least one annotation shown.")
                               ),
                               column(3, style="border-left: 1px solid lightgray",
                                      div(class = "forceInline", selectizeInput("Clist", "Phenotype/clinical variable(s)", 
                                                                                choices = Columns[Source %in% c("Aab", "Demographics", "DiabetesInfo"), Variable], 
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
                      ),
                      column(12, 
                             tableOutput("debugtable"))
             )
             ),
             
             #-- PAGE 4 ----------------------------------------------------------------------------------------#
             tabPanel("Stories", value = "stories" #icon = icon("asterisk")
                      
             ),
             
             #-- PAGE 5 ----------------------------------------------------------------------------------------#
             tabPanel("Source Data", value = "source-data", # icon = icon("database"),
                      checkboxInput("filterDT", "Only display sources where individual-level data is readily available.", value = T, width = 500),
                      helpText("'Get from original source' link points to the original data in a supplemental file 
                               or to an external database where data has been deposited. The original sources can provide more detail about
                               methodology, definitions and other metadata, but are in a variety of formats not universally machine-readable (e.g. PDF, Excel). 
                               To facililate re-use, curated data can also be downloaded all at once (except for some high-throughput datasets) 
                               as a collection of plain text tab-delimited  files."),
                      downloadButton("download", label = "Download Archive"),
                      DT::dataTableOutput("sourceDT")
             )
))
