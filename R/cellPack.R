#' Shiny UI widget module for visualization and filtering by cell and tissue
#'
#' Based on d3 circlepack.js, this represents cells and tissues as circles placed in a
#' hierarchy.
#'
#' Certain selections can trigger the widget "augmented input interface".
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
cellPackUI <- function(id, width = "400px", height = "400px") {
  ns <- NS(id)
  r2d3::d3Output(ns("cellpack"), width = width, height = height)
}

#' Shiny server module function for cellPack widget
#'
#' Shiny server module function for cellPack widget
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param json Either a JSON object or relative path to the JSON data file for widget.
#' @param css Optional, relative path to a CSS file.
#' @return Reactive object for element that was clicked
#' @export
cellPack <- function(input, output, session,
                     json, css = system.file("www/", "cellpack.css", package = "DIVE")) {

  output$cellpack <- r2d3::renderD3({
    r2d3::r2d3(data = jsonlite::read_json(json), script = system.file("www/", "cellpack.js", package = "DIVE"),
               options = list(click = session$ns("click"), hover = session$ns("hover")),
               css = css, d3_version = 4, viewer = "browser")
  })

  return(reactive({ input$click }) )
}

