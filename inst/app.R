library(shiny)
library(DIVE)


ui <- navbarPage("nPOD DIVE", id = "main", selected = "intro",
                 theme = shinythemes::shinytheme("lumen"),
                 tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "introjs.min.css"),
                           tags$script(src = "intro.min.js")),
                 includeScript("www/app.js"),

  #-- MENU PG 1 (HOME) ---------------------------------------------------------------------------------#
  tabPanel("Connections in Investigations", value = "home",
           fluidPage()
  ),
  #-- MENU PG 2,3,4 ------------------------------------------------------------------------------------#
  navbarMenu("Integrative Data Views",
             tabPanel("Cohort Exchange", value = "cohort-exchange",
                      actionButton("guideMatch", "Demo tour", icon = icon("info-circle")),
                      matchAppUI("match")),
             tabPanel("Data Exploration", value = "data-exploration-1",
                      actionButton("guideCorrelation", "Demo tour", icon = icon("info-circle")),
                      interactiveMatrixAppUI("module2")),
             tabPanel("Data Exploration (high-throughput)", value = "data-exploration-2",
                      "")
  ),
  #-- MENU PG 5 ----------------------------------------------------------------------------------------#
  tabPanel("Vignettes", value = "stories" #icon = icon("asterisk")

  ),

  #-- MENU PG 6 ----------------------------------------------------------------------------------------#
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

server <- function(input, output, session) {
  callModule(matchApp, "match")
  callModule(interactiveMatrixApp, "module2")
  observeEvent(input$guideCorrelation, {
    session$sendCustomMessage(type = "startGuideC",
                              message = list(steps = jsonlite::toJSON(read.table("help/data_exploration.txt", sep = "\t", header = T, comment.char = ""))))
  })
  observeEvent(input$guideMatch, {
    session$sendCustomMessage(type = "startGuideM",
                              message = list(steps = jsonlite::toJSON(read.table("help/cohort_exchange.txt", sep = "\t", header = T, comment.char = ""))))
  })
}

shinyApp(ui = ui, server = server)
