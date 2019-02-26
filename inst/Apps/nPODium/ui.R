library(DIVE)

navbarPage("nPOD DIVE", id = "main", selected = "intro",
           theme = shinytheme("lumen"), tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
                                                  tags$link(rel = "stylesheet", type = "text/css", href = "introjs.min.css"),
                                                  tags$script(src = "intro.min.js")),
           includeScript("www/app.js"),

           #-- PAGE 1 ----------------------------------------------------------------------------------------#
           tabPanel("Connections in Investigations", value = "intro", # icon = icon("connectdevelop"),
                    fluidRow(
                      column(8,
                             div(id = "nPOD-connections",
                                 # visNetworkOutput("network", height = "750px"),
                                 actionButton("accessdata", "Data accessible", icon = icon("circle", class = "node")),
                                 actionButton("noaccessdata", "Data not accessible", icon = icon("circle", class = "node"))
                             )
                      ),
                      column(4,
                             div(id = "about", includeMarkdown("Intro.Rmd"))
                      )
                    )),
           #-- PAGE 2 ----------------------------------------------------------------------------------------#
           navbarMenu("Integrative Data Views",

                      tabPanel("Cohort Exchange", value = "data-fusion-1", # icon = icon("cube"),
                               ""
                      ),


                      #-- PAGE 3 ----------------------------------------------------------------------------------------#
                      tabPanel("Experimental data", value = "data-fusion-2",
                               ""
                      ),
                      #-- PAGE 4 ----------------------------------------------------------------------------------------#
                      tabPanel("Experimental data (high-throughput)", value = "HD", # icon = icon("cubes"),
                               ""
                      )),
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
)
