library(shiny)
library(DIVE)


ui <- navbarPage("nPOD DIVE", id = "main", selected = "intro", collapsible = T,
                 theme = shinythemes::shinytheme("paper"),
                 tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
                           tags$link(rel = "stylesheet", type = "text/css", href = "introjs.min.css"),
                           tags$script(src = "intro.min.js")),
                 includeScript("www/app.js"),

  #-- MENU PG 1 (HOME) ---------------------------------------------------------------------------------#
  tabPanel("Connections in Investigations", value = "home",
           fluidPage()
  ),
  #-- MENU PG 2,3,4 ------------------------------------------------------------------------------------#
  navbarMenu("Integrative Data Views",
             tabPanel("Cohort Exchange", value = "cohort-exchange",
                      actionButton("demoCohortExchange", "Help Demo", icon = icon("play")),
                      matchAppUI("match", CSS = NULL)),
             tabPanel("Data Exploration", value = "data-exploration-1",
                      actionButton("demoCorrelation", "Help Demo", icon = icon("play")),
                      interactiveMatrixAppUI("cor", CSS = NULL)),
             tabPanel("Data Exploration (high-throughput)", value = "data-exploration-2",
                      multiVUIApp("default"))
  ),
  #-- MENU PG 5 ----------------------------------------------------------------------------------------#
  tabPanel("Vignettes", value = "stories" #icon = icon("asterisk")

  ),

  #-- MENU PG 6 ----------------------------------------------------------------------------------------#
  tabPanel("Browse and Download", value = "source-data", # icon = icon("database"),
           browseUI("metadata")
  ),

  #-- PAGE 7 ----------------------------------------------------------------------------------------#
  tabPanel("Give Data", value = "give", # icon = icon("database"),
           ""
  )
)

server <- function(input, output, session) {

  callModule(matchApp, "match")
  callModule(interactiveMatrixApp, "cor")
  callModule(multiVApp, "default")

  observeEvent(input$demoCorrelation, {
    session$sendCustomMessage(type = "demoCorrelation",
                              message = list(steps = jsonlite::toJSON(read.table("help/data_exploration.txt", sep = "\t", header = T, comment.char = ""))))
  })
  observeEvent(input$demoCohortExchange, {
    session$sendCustomMessage(type = "demoCohortExchange",
                              message = list(steps = jsonlite::toJSON(read.table("help/cohort_exchange.txt", sep = "\t", header = T, comment.char = ""))))
  })
  callModule(browse, "metadata")
}

shinyApp(ui = ui, server = server)
