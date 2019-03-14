#' Shiny module UI for controlling multi-column view
#'
#' Shiny module UI for controlling multi-column view
#' 
#' Multiple high dimensional datasets can be represented by multiple \code{\link{multiVUI}} modules.
#' This module controls selection/sourcing of the available datasets. 
#' 
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
multiVCtrlUI <- function(id) {
  ns <- NS(id)
  fluidRow(style="margin-top:30px; margin-bottom:20px; margin-right:100px",
           div(class = "forceInline", selectizeInput(ns("dataset"), "nPOD high-throughput datasets", 
                                      choices = NULL, selected = NULL, 
                                      options = list(maxItems = 50, placeholder = "selection"))),
           div(class = "forceInline", textInput(ns("GEO"), "Use public dataset from GEO", placeholder = "enter the GSE id")),
           div(class = "forceInline", dataUploadUI(ns("hdata"))
           )
  )
}

#' Shiny module server for controlling multi-column view
#'
#' Implements sourcing datasets for \code{\link{multiVUI}} modules in the application page.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @return A data matrix for the parameter \preformatted{hdata} in the \code{\link{multiV}} module.
#' @export
multiVCtrl <- function(input, output, session
                      ) {
  
  hdata <- reactiveVal()
  
  observeEvent(input$dataset, {
    
  })
  callModule(dataUpload, "hdata")
  
}