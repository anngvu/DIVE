library(shiny)
library(DIVE)

shinyUI(
  navbarPage("nPOD DIVE", id = "main", selected = "home", collapsible = T,
             theme = shinythemes::shinytheme("paper"),
             tags$head(# tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
               tags$link(rel = "stylesheet", type = "text/css", href = "introjs.min.css")),
             includeScript("www/intro.min.js"),
             includeScript("www/app.js"),

             #-- MENU PG 1 (HOME) ---------------------------------------------------------------------------------#
             tabPanel("Home", value = "home",
                      fluidRow(# style = "margin-top: -70px;",
                        column(6, style = "margin-left: -25px; margin-top: -50px;",
                               div(shinycssloaders::withSpinner(visNetwork::visNetworkOutput("studynetwork", height = "900px")))
                               ),
                        column(6, style = "padding-right: 100px;",
                               # summary widgets
                               fluidRow(
                                 htmlOutput("n_studies"),
                                 htmlOutput("n_measures"),
                                 htmlOutput("n_medianshared"),
                                 htmlOutput("n_openaccess"),
                                 htmlOutput("n_htdatasets"),
                                 htmlOutput("t_last")
                               ),
                               fluidRow(
                                 tabsetPanel(
                                   tabPanel("Connections in Investigations", br(), includeMarkdown("www/connections.Rmd")),
                                   tabPanel("Project", br(), includeMarkdown("www/about.Rmd"))
                                )
                              )
                        )
                      )
             ),
             #-- MENU PG 2,3,4 ------------------------------------------------------------------------------------#
             navbarMenu("Integrative Data Views",
                        tabPanel("Cohort Exchange", value = "cohort-exchange",
                                 actionButton("demoCohortExchange", "Guide", icon = icon("play")),
                                 matchAppUI("match", CSS = NULL)),
                        tabPanel("Data Exploration", value = "data-exploration-1",
                                 actionButton("demoCorrelation", "Guide", icon = icon("play")),
                                 interactiveMatrixAppUI("cor", CSS = NULL)),
                        tabPanel("Data Exploration (high-throughput)", value = "data-exploration-2",
                                 multiVUIApp("default"))
             ),
             #-- MENU PG 5 ----------------------------------------------------------------------------------------#
             # tabPanel("Vignettes", value = "stories" #icon = icon("asterisk")
             #
             # ),

             #-- MENU PG 6 ----------------------------------------------------------------------------------------#
             tabPanel("Browse Data", value = "source-data", # icon = icon("database"),
                      browseUI("nPOD")
             ),

             #-- PAGE 7 ----------------------------------------------------------------------------------------#
             tabPanel("Give Data", value = "give", # icon = icon("database"),
                      ""
             )
  )
)
