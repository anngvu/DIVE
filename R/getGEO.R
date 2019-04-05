#' Shiny module UI for importing data from GEO
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
getGEOInput <- function(id, infoRmd = system.file("help/GEO_module.Rmd", package = "DIVE")) {
  ns <- NS(id)
  tags$div(id = "getGEOInput",
           textInput(ns("GSE"), label = "Enter a GSE ID (i.e. GSE72492)"),
           includeMarkdown(infoRmd)
           )
}

#' Shiny module server for importing data from GEO
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @export
getGEOMod <- function(input, output, session) {
  
  # Pull GEO with given GSE
  
  # Checks: species is human, 1-channel data
  
  # Get metadata
  
  # 
  
}