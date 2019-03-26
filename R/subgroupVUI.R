#' Shiny app UI for creating comparison plots between subgrouped data
#'
#'
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
subgroupVUI <- function(id, hdchoices = NULL, cchoices = NULL) {

  ns <- NS(id)
  tags$div(id = ns("viewport"), class = "subgroups-panel",
           div(align = "right", actionButton(ns("remove"), "", icon = icon("times"))),
           # verbatimTextOutput(ns("test")),
           selectizeInput(ns("hdataset0"), "HT dataset", choices = hdchoices, selected = NULL, multiple = F),
           selectInput(ns("groupby1"), "(A) group by", choices = removeID(cchoices)),
           uiOutput(ns("by1values"), inline = T),
           selectInput(ns("groupby2"), "(B) group by", choices = removeID(cchoices)),
           uiOutput(ns("by2values"), inline = T)
  )
}

#' Shiny module server for creating comparison plots between subgrouped data
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param cdata
#' @param hdata A list of named high-dimensional (high-throughput datasets)
#' @return
#' @export
subgroupV <- function(input, output, session,
                      cdata, hdata) {

  # updateSelectizeInput(session, "hdataset0", choices = LETTERS[1:5], selected = NULL)

  # output$test <- renderPrint({
  #   names(hdata)
  # })

  # output$by1values <- renderUI({
  #   if(is.null(input$groupby1)) return(NULL)
  #   x <- cdata[[input$groupby2]]
  #   if(class(x) == "character" | class(x) == "factor") {
  #     return(selectizeInput(session$ns("values2"), label = "with factor", choices = unique(x)))
  #   } else {
  #     return(sliderInput(session$ns("values2"), label = "with range",
  #                        min = min(x, na.rm = T), max = max(x, na.rm = T),
  #                        value = c(min(x, na.rm = T), max(x, na.rm = T))))
  #   }
  # })
  #
  # output$by2values <- renderUI({
  #   if(is.null(input$groupby2)) return(NULL)
  #   x <- cdata[[input$groupby2]]
  #   if(class(x) == "character" | class(x) == "factor") {
  #     return(selectizeInput(session$ns("values1"), label = "with factor", choices = unique(x)))
  #   } else {
  #     return(sliderInput(session$ns("values1"), label = "with range",
  #                        min = min(x, na.rm = T), max = max(x, na.rm = T),
  #                        value = c(min(x, na.rm = T), max(x, na.rm = T))))
  #   }
  # })

  observeEvent(input$remove, {
    removeUI(selector = paste0("#", session$ns("viewport")))
  }, once = T)

}
