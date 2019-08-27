library(shiny)
library(DIVE)

shinyServer(function(input, output, session) {

  callModule(matchApp, "match")
  callModule(interactiveMatrixApp, "cor")
  callModule(multiVApp, "default")
  callModule(browse, "nPOD")

  output$n_studies <- renderUI({
    n <- length(unique(metadata$Source))
    DIVE::valueBox(value = n, subtitle = "CURATED DATA SOURCES", icon = icon("book-open"), width = 4, textcolor = "white", bgcolor = "#2196f3")
  })

  output$n_openaccess <- renderUI({
    n <- sum(studynetwork$x$nodes$color != "#333333")
    DIVE::valueBox(value = n, subtitle = "OPEN-DATA SOURCES", icon = icon("lock-open"), width = 4, textcolor = "white", bgcolor = "#2196f3")
  })

  output$n_htdatasets <- renderUI({
    n <- sum(metadata$Dimensions > 300, na.rm = T)
    DIVE::valueBox(value = n, subtitle = "HIGH-THROUGHPUT DATASETS", icon = icon("table"), width = 4, textcolor = "white", bgcolor = "mediumspringgreen")
  })

  output$n_measures <- renderUI({
    n <- sum(metadata$Dimensions == 1, na.rm = T)
    DIVE::valueBox(value = n, subtitle = "SMALL-THROUGHPUT FEATURES", icon = icon("microscope"), width = 4, textcolor = "white", bgcolor = "mediumspringgreen")
  })

  output$n_medianshared <- renderUI({
    n <- round(mean((studynetwork$x$edges$weight)^2), 1)
    DIVE::valueBox(value = n, subtitle = "MEAN SHARED CASES", icon = icon("share-alt"), width = 4, textcolor = "white", bgcolor = "MediumVioletRed")
  })

  output$t_last <- renderUI({
    DIVE::valueBox(value = "--/--/--", subtitle = "MOST RECENT SOURCE", icon = icon("copy"), width = 4, textcolor = "white", bgcolor = "MediumVioletRed")
  })


  output$studynetwork <- visNetwork::renderVisNetwork({
    studynetwork %>%
      visNetwork::visIgraphLayout(randomSeed = 98) %>%
      visNetwork::visInteraction(hover = T) %>%
      visNetwork::visNodes(font = list(size = 20, face = "Helvetica", background = "white")) %>%
      visNetwork::visEdges(color = list(highlight = "#ff0000", hover = "#ff0000", inherit = F)) %>%
      visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = T))
  })

  observeEvent(input$demoCorrelation, {
    session$sendCustomMessage(type = "demoCorrelation",
                              message = list(steps = jsonlite::toJSON(fread("www/data_exploration.txt", sep = "\t", header = T))))
  })

  observeEvent(input$demoCohortExchange, {
    session$sendCustomMessage(type = "demoCohortExchange",
                              message = list(steps = jsonlite::toJSON(fread("www/cohort_exchange.txt", sep = "\t", header = T))))
  })

})
