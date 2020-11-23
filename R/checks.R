# These are all data check functions used **internally** by various modules
# Other types of user-facing check functions in pChecks.R (TO-DO)

#' Check cohort data
#'
#' Check that data has an ID column and that data is numeric/numeric factors
#' @keywords internal
checkCohortData <- function(cohdata, message = NULL, result = NULL) {
  hasID <- "ID" %in% names(cohdata)
  if(!hasID) message <- "<strong>There is no ID column.</strong><br>"
  notID <- removeID(names(cohdata))
  isNumeric <- sapply(cohdata[, notID, with = F], function(x) class(x) %in% c("numeric", "integer"))
  if(!all(isNumeric)) message <- paste(message,
                                       paste("<strong>These columns must be numeric (encode categorical data):</strong><br>",
                                             paste(notID[!isNumeric], collapse = "<br>")))
  if(is.null(message)) result <- cohdata
  return(list(result = result, message = message))
}


#' General check function for user-uploaded data
#'
#' This is used in \code{\link{dataUploadServer}}. Checks for (in this order):
#' \enumerate{
#'   \item exactly one ID column, which is by default converted to character
#'   \item actual data columns besides ID
#'   \item all data columns are numeric
#' }
#' When all of the above pass without issues, the function returns a named list
#' with data contained in \code{result} and \code{NULL} contained in \code{message},
#' otherwise it witholds the data by returning \code{NULL} for \code{result} and
#' \code{message} will contain a character vector of the issue(s) noted.
#' If the downstream use for data requires different check steps and transformations,
#' this function can be replaced as long as the new one uses the same return structure.
#' For some cases, if it is desired that we return data even if there are issues,
#' use \code{result = data} and rely on the \code{message} output.
#'
#' @param data A \code{data.table}.
#' @param result What to return in place of data if issues are found with the data. Defaults to \code{NULL}.
#' @param idcol Name of expected ID column, defaulting to "ID".
#' @param idchar Logical with default \code{TRUE}, for whether to convert \code{idcol} to character.
#' @param html (Not used) Logical with default TRUE, for whether to format message with HTML for display.
#' @return A list with \code{result} and \code{message}. See details.
#' @import data.table
#' @keywords internal
checkDataUpload <- function(data,
                            result = NULL,
                            idcol = "ID", idchar = TRUE, html = TRUE) {
  message <- NULL
  whichID <- which(idcol %in% names(data))
  if(length(whichID) != 1) {
    message <- paste("Need to have exactly one column with IDs called", idcol)
  }
  if(length(data[, !whichID, with = F])) {
      notnumeric <- sapply(data[, !whichID, with = F], function(x) !is.numeric(x))
      if(length(which(notnumeric))) message <- c(message, "All data columns should be numeric/numeric factor.")
    } else {
      message <- c(message, "Data columns appear to be missing.")
  }
  if(is.null(message)) {
    if(idchar) data[[idcol]] <- as.character(data[[idcol]])
    result <- data
  }
  return(list(result = result, message = message))
}

#' Check and make character ID column
#'
#' Checks for some sort of ID column, which is by default converted to character.
#' Returns a message for failures.
#' This is meant to work with dataset upload modules.
#'
#' @param data A table.
#' @param message A default message for failures.
#' @param idcol Name of ID column, defaulting to "ID".
#' @param aschar Convert ID column to character.
#' @return A list with elements result with the result data and message with a message to show.
#' @keywords internal
checkForID <- function(data, message = NULL, idcol = "ID", aschar = T) {
  hasID <- idcol %in% names(data)
  if(!hasID) message <- "<strong>There is no ID column.</strong><br>"
  if(hasID && aschar) data[[idcol]] <- as.character(data[[idcol]])
  return(list(message = message, result = data))
}
