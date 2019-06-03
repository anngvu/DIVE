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

# These are all the data check functions used internally

# Checks that data has an ID column and that data is numeric/numeric factors
checkCohortData <- function(cohdata, message = NULL, result = NULL) {
  hasID <- "ID" %in% names(cohdata)
  if(!hasID) message <- "<strong>There is no ID column.</strong><br>"
  notID <- removeID(names(cohdata))
  isNumeric <- sapply(cohdata[, notID, with = F], function(x) class(x) %in% c("numeric", "integer"))
  if(!all(isNumeric)) message <- paste(message,
                                       paste("<strong>These columns must be numeric/factor-encoded data:</strong><br>",
                                             paste(notID[!isNumeric], collapse = "<br>")))
  if(is.null(message)) result <- cohdata
  return(list(message = message, result = result))
}

#' Returns a list of IDs found in reference list
xCheckID <- function(data, ref = cdata$ID) {
  IDs <- data$ID[which(data$ID %in% ref)]
  return(IDs)
}
