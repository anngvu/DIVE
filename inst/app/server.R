library(shiny)
library(DIVE)

shinyServer(function(input, output, session) {

  callModule(matchApp, "match")
  callModule(interactiveMatrixApp, "cor")
  callModule(multiVApp, "default")
  callModule(browse, "nPOD")

  output$studynetwork <- renderVisNetwork({
    studynetwork %>%
      visIgraphLayout(randomSeed = 98) %>%
      visInteraction(hover = T) %>%
      visNodes(font = list(size = 20, face = "Helvetica", background = "white")) %>%
      visEdges(color = list(highlight = "#ff0000", hover = "#ff0000", inherit = F)) %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = T))
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
