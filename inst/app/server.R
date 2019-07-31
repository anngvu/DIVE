library(shiny)
library(DIVE)

shinyServer(function(input, output, session) {
  
  callModule(matchApp, "match")
  callModule(interactiveMatrixApp, "cor")
  callModule(multiVApp, "default")
  callModule(browse, "nPOD")
  
  observeEvent(input$demoCorrelation, {
    session$sendCustomMessage(type = "demoCorrelation",
                              message = list(steps = jsonlite::toJSON(fread("www/data_exploration.txt", sep = "\t", header = T))))
  })
  
  observeEvent(input$demoCohortExchange, {
    session$sendCustomMessage(type = "demoCohortExchange",
                              message = list(steps = jsonlite::toJSON(fread("www/cohort_exchange.txt", sep = "\t", header = T))))
  })
  
})