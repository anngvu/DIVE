library(shinythemes)
library(shinycssloaders)

shinyUI(navbarPage("nPOD DIVE", id = "main", selected = "intro", 
                   theme = shinytheme("lumen"), tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                   
#-- PAGE 1 ----------------------------------------------------------------------------------------#                 
  tabPanel("Connections in Investigations", value = "intro",
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
  tabPanel("2D", value = "page-2", fluidPage(
           fluidRow(
             column(1,
                    br(),
                    actionButton("helpCorrelation", "Help/Info")
             ),
             column(1, 
                    numericInput("minimumN", HTML("min. N for <i>r</i>"), min = 2, max = NA, step = 1, val = 5, width = "80px")
             ),
             column(2, 
                    selectizeInput("varExclude", "Exclude variables from correlation matrix", cdata.vars, multiple = T)
             ),
             # column(1, 
             #        br(),
             #        actionButton("varReset", "Reset")
             # ),
             column(3, 
                    div(class = "forceInline", br(), actionButton("helpUpload", "", icon = icon("question-circle"))),
                    div(class = "forceInline", fileInput("dataUpload", "", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                                  buttonLabel = "My data...", placeholder = "Upload data for comparison"))
             ),
             column(4, 
                    selectizeInput("drilldown", "Drill down to data points for V1 or V1 x V2:", 
                                   choices = c("", unique(corM$Var1)), selected = "", options = list(maxItems = 2))
             ),
             column(1, 
                    conditionalPanel("input.drilldown",
                                     selectInput("colorby", "Color data points by", choices = names(cdata)[!names(cdata) %in% "ID"], selected = "donorType", width = "200px"),
                                     checkboxInput("plotsmooth", "Add smooth"))
             )),
           fluidRow(
             column(8, align = "center",
                    plotlyOutput("corM")
             ),
             column(4, 
                    plotlyOutput("scatter")
             ))
          )),

#-- PAGE 3 ----------------------------------------------------------------------------------------#
  tabPanel("HD", value = "page-3",
           fluidRow(class = "top-options",
             column(2,
                    br(),
                    actionButton("helpVolcano", "Help/Info"),
                    actionButton("viewVolcano", "Show/Hide Volcano(s)")
             ),       
             column(2,
                    selectizeInput("Glist", "Selected genes (proteins) list", choices = NULL, selected = NULL, options = list(maxItems = 20))
             ),
             column(2,
                    actionButton("highlight", "Highlight selected"),
                    actionButton("volcanoReset", "Reset"),
                    helpText("Note: Expression may not be available in all data.")
             ),
             column(2, align="right", 
                    radioButtons("GorR", "Or genes/gene products annotated to:", choices = c("Gene Ontology", "Reactome"),
                                 selected = "Gene Ontology", inline = T),
                    conditionalPanel(condition = "input.GorR == 'Gene Ontology'",
                                     radioButtons("BPCCMP", "GO branch",
                                                  choices = c("Biological Process" = "BP", "Cellular Component" = "CC", "Molecular Function" = "MF"),
                                                  selected = "BP", inline = T))
             ),
             column(2,
                    selectizeInput("GOReactq", "GO Term", choices = NULL, selected = NULL)
             ),
             column(2,
                    selectizeInput("Plist", "Phenotype/clinical variable(s)", choices = unique(Columns$MPOLabel), selected = "", options = list(maxItems = 20))
          )),
          fluidRow(
            column(12,
                   conditionalPanel("input.viewVolcano%2==1",
                     checkboxGroupInput("activeVolcano", "Showing data for:", 
                                        choiceNames = c("Transcriptomics | AAB-HC (Yip et et. unpublished)", "Transcriptomics | T1D-HC (Yip et et. unpublished)", 
                                                        "Proteomics | T1D-HC (Liu et al. 2016)", "Proteomics | T1D-HC (Nyalwidhe et al. 2017)", "Proteomics | AAB-HC (Nyawidle et al. 2016)"),
                                        choiceValues = c("gx.AAB", "gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                        selected = c("gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                        inline = T)
                   )
            )),
          fluidRow(
            column(12, 
                   withSpinner(plotlyOutput("volcanos"), type = 4, color = "gray"))
          ),
          fluidRow(
            column(12,
                   plotlyOutput("xGOReact")
          )
          )),

#-- PAGE 4 ----------------------------------------------------------------------------------------#
  tabPanel("Stories", value = "stories"),

#-- PAGE 5 ----------------------------------------------------------------------------------------#
  tabPanel("Source Data", value = "source-data",
           checkboxInput("filterDT", "Only display sources where individual-level data is readily available.", value = T, width = 500),
           DT::dataTableOutput("sourceDT")
          )
))
