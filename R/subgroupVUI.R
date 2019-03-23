#' Shiny module UI for comparing high-dimensional data by subgroups
#'
#' Inserts a panel UI of the requested data view comparison
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
subgroupVUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "subgroupVUI",
           actionButton(ns("new"), " Subgroup view", icon = icon("object-ungroup")),
           absolutePanel(uiOutput(ns("view")), draggable = T)
  )
}

#' Shiny module server for comparing high-dimensional data by subgroups
#'
#' Handles creation of a panel UI of the requested data view comparison
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @export
subgroupV <- function(input, output, session,
                                   subgroups = NULL) {
   
  observeEvent(input$new, {
    insertUI(paste0("#", session$ns("view")), 
             ui = tags$div(class = "absdiv", style = "background: white; padding: 10px; border: 1px solid black;",
                      h4("Subgroup selection"), 
                      selectInput(session$ns("which"), "Which", choices = "")
                      )
    )
  })
}