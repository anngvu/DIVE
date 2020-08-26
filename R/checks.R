# These are all the data check functions used internally

#' Check match data
#' Checks that data has an ID column and that data is numeric/numeric factors
checkCohortData <- function(cohdata, message = NULL, result = NULL) {
  hasID <- "ID" %in% names(cohdata)
  if(!hasID) message <- "<strong>There is no ID column.</strong><br>"
  notID <- removeID(names(cohdata))
  isNumeric <- sapply(cohdata[, notID, with = F], function(x) class(x) %in% c("numeric", "integer"))
  if(!all(isNumeric)) message <- paste(message,
                                       paste("<strong>These columns must be numeric (encode categorical data):</strong><br>",
                                             paste(notID[!isNumeric], collapse = "<br>")))
  if(is.null(message)) result <- cohdata
  return(list(message = message, result = result))
}

#' xCheckID
#' Also used for match module
#' Returns a list of IDs found in reference list
#' #' Tells user that internal matches have been found
xCheckID <- function(data, ref = cdata$ID) {
  IDs <- data$ID[which(data$ID %in% ref)]
  return(IDs)
}

#' General check for uploaded data
#'
#' This is generally meant for dataset upload modules. Checks for (in this order):
#' \enumerate{
#'   \item one ID column, which is by default converted to character
#'   \item actual data columns besides ID
#'   \item all data columns are numeric
#' }
#' If none of the above, returns the data in \code{result} and NULL in \code{message},
#' otherwise the method witholds the data by returning NULL and
#' message will be non-NULL, containing the issue noted.
#' For some use cases might want return data even if there are issues
#' by specifying \code{result = data}.
#'
#' @param data A data.table.
#' @param result What to return in place of data if issues are found with the data.
#' @param idcol Name of ID column, defaulting to "ID".
#' @param idchar Logical with default TRUE, whether to convert ID column to character.
#' @param html Logical with default TRUE, whether to format message for HTML display.
#' @return A list with  \code{result} and  \code{message}.
#' @export
checkDataUpload <- function(data, result = NULL,
                      idcol = "ID", idchar = TRUE, html = TRUE) {
  message <- NULL
  whichID <- which(idcol %in% names(data))
  if(length(whichID) != 1) {
    message <- paste("Need to have exactly one column with ids called", idcol)
  } else {
    if(idchar) data[[idcol]] <- as.character(data[[idcol]])
    if(length(data[, !..whichID])) {
      notNumeric <- sapply(data[, !..whichID], function(x) !is.numeric(x))
      nn <- length(which(notNumeric))
      if(nn) message <- c(message, "All data columns should be numeric/numeric factor.")
    } else {
      message <- c(message, "Data columns appear to be missing.")
    }
  }
  if(is.null(message)) result <- data
  return(list(result = result, message = message))
}

#' Check and make character ID column
#'
#' Checks for some sort of ID column, which is by default converted to character.
#' Returns a message for failures.
#' This is meant to work with dataset upload modules.
#'
#' @param data A table.
#' @param idcol Name of ID column, defaulting to "ID".
#' @param aschar Convert ID column to character.
#' @return A list with elements result with the result data and message with a message to show.
checkForID <- function(data, message = NULL, idcol = "ID", aschar = T) {
  hasID <- idcol %in% names(data)
  if(!hasID) message <- "<strong>There is no ID column.</strong><br>"
  if(hasID && aschar) data[[idcol]] <- as.character(data[[idcol]])
  return(list(message = message, result = data))
}
