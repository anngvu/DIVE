#' UI widget for visualization and filtering of hierarchical cell and tissue samples
#'
#' Based on d3 circlepack.js
#'
#' Certain selections can trigger the widget "augmented input interface"
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
cellPackUI <- function(id, width = "400px", height = "400px") {
  ns <- NS(id)
  tagList(fluidRow(d3Output(ns("cellpack"), width = width, height = height)),
          fluidRow(textOutput(ns("out")))
          )
}

#' Server function to render cellPack widget
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param json Relative path to the JSON data for widget.
#' @param css Optional, relative path to a CSS file.
#' @return return input id javascript values can be accessed with (id)_click and (id)_hover
#' @export
cellPack <- function(input, output, session,
                     json, css = system.file("www/", "cellpack.css", package = "DIVE")) {

  output$cellpack <- renderD3({
    r2d3::r2d3(data = read_json(json), script = system.file("www/", "cellpack.js", package = "DIVE"),
               options = list(click = session$ns("click"), hover = session$ns("hover")),
               css = css, d3_version = 4, viewer = "browser")
  })

  return(reactive({ input$click }) )
}

