library(shiny)
library(DIVE)

shinyServer(function(input, output, session) {

  callModule(matchApp, "match")
  callModule(interactiveMatrixApp, "cor")
  callModule(multiVApp, "default")
  callModule(browse, "nPOD")

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
