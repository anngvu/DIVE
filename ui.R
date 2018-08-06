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
                    numericInput("n_minimum", HTML("min. N for <i>r</i>"), min = 2, max = NA, step = 1, val = 5, width = "80px")
             ),
             column(2, 
                    selectizeInput("var_exclude", "Exclude variables from correlation matrix", cdata.vars, multiple = T)
             ),
             # column(1, 
             #        br(),
             #        actionButton("reset_vars", "Reset")
             # ),
             column(3, 
                    div(class = "forceInline", br(), actionButton("helpNew", "", icon = icon("question-circle"))),
                    div(class = "forceInline", fileInput("newdata", "", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                                  buttonLabel = "My data...", placeholder = "Upload data for comparison"))
             ),
             column(4, 
                    selectizeInput("drilldown", "Drill down to data points for V1 or V1 x V2:", 
                                   choices = c("", unique(corM$Var1)), selected = "", options = list(maxItems = 2))
             ),
             column(2, 
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
  tabPanel("3D", value = "page-3",
           fluidRow(
            column(1, 
                   actionButton("volcanoInfo", "Help/Info")
            ),
            column(11,
                   checkboxGroupInput("activeVolcano", "Showing data for:", 
                                      choiceNames = c("Transcriptomics|AAB vs HC (Yip et et. unpublished)", "Transcriptomics|T1D vs HC (Yip et et. unpublished)", 
                                                       "Proteomics|T1D vs HC (Liu et al. 2015)", "Proteomics|T1D vs HC (Nyawidle et al. 2016)", "Proteomics| AAB vs HC (Nyawidle et al. 2016)"),
                                      choiceValues = c("gx.AAB", "gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                      selected = c("gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                      inline = T)
            )),
           hr(),
           fluidRow(class = "top-options",
             column(2,
                    selectizeInput("Glist", "Selected genes list", choices = NULL, selected = NULL, options = list(maxItems = 20))
             ),
             column(1,
                    br(),
                    actionButton("highlight", "Highlight selected")
             ),
             column(2,
                    radioButtons("GorR", "Or get genes/gene products annotated to:", choices = c("Gene Ontology", "Reactome"),
                                 selected = "Gene Ontology", inline = T)
             ),
             column(2,
                    selectizeInput("GOReactq", "GO term", choices = NULL, selected = NULL)
             ),
             column(3,
                    conditionalPanel(condition = "input.GorR == 'Gene Ontology'",
                                     checkboxGroupInput("BPCCMP", "GO branch",
                                                               choices = c("Biological Process" = "BP", "Cellular Component" = "CC", "Molecular Function" = "MF"),
                                                               selected = "BP", inline = T))
             ),
             column(2,
                    selectizeInput("Plist", "Phenotype/clinical variables", choices = unique(Columns$MPOLabel), selected = "", options = list(maxItems = 20))
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
