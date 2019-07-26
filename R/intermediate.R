#' Shiny module UI for additional intermediate input
#'
#' Adds some UI to provide input for additional data processing
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return A \code{shiny::\link[shiny]{tagList}} containing input UI.
#' @export
intermediateInput <- function(id) {
  ns <- NS(id)
}


#' Shiny server module for intermediate data processing
#'
#' This module implements an additional customizable layer for intermediate data processing
#' by passing data into a function and returning results as a reactive object.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param data Reactive data.
#' @param Fun Function to be applied to data.
#' @return Return value of Fun as reactive object.
#' @export
intermediate <- function(input, output,
                      data, Fun) {

  out <- reactive({
    Fun(data())
  })

  return(out)
}


