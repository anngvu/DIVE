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
intermediate <- function(input, output, session,
                      data, Fun = NULL) {

  reactive({
    if(is.function(Fun)) Fun(data()) else return(NULL)
  })

}


