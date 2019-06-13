#' Shiny module UI for additional intermediate input
#'
#' Adds some UI to provide input for additional data processing
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return A \code{shiny::\link[shiny]{tagList}} containing input UI.
#' @export
intermediateInput <- function(id) {
  ns <- NS(id)
  fluidRow(

  )
}


#' Shiny server module for intermediate data processing
#'
#' This module implements an additional customizable layer for intermediate data processing
#' by passing data into a function and outputting results
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param data Reactive data.
#' @param Fun Function for data processing.
#' @return Reactive object that depends on the return of the function used.
#' @export
intermediate <- function(input, output, session,
                      data, Fun) {

  out <- reactive({
    Fun(data())
  })

  return(out)
}


