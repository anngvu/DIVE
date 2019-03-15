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
  tags$div(
    fluidRow(style="margin-top:30px; margin-bottom:20px; margin-right:100px",
           div(class = "forceInline", selectInput(ns("dataset"), "nPOD high-throughput datasets", 
                                      choices = NULL, selected = NULL)),
           div(class = "forceInline", dataUploadUI(ns("hdata")))
    ),
    fluidRow(selectizeInput(ns("inview"), "In view", choices = NULL))
  )
}

#' Shiny module server for controlling multi-column view
#'
#' Implements sourcing datasets for \code{\link{multiVUI}} modules in the application page.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param hdlist A list containing high dimensional datasets; 
#' the names of each element are used as choices in the selection menu.
#' @return A data matrix for the parameter \preformatted{hdata} in the \code{\link{multiV}} module.
#' @export
multiVCtrl <- function(input, output, session,
                      hdlist) {
  
  view <- reactiveVal(NULL)
  
  updateSelectInput(session, "dataset", choices = c("", names(hdlist)))
  
  observeEvent(input$dataset, {
    if(input$dataset != "") {
      view(hdlist[[input$dataset]])
    } 
    
  })
  
  return(view)
}