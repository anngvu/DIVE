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

#' Check and make character ID column
#'
#' Checks for some sort of ID column, which can optionally be converted to character. Returns a message for failures.
#' This is meant to work with dataset input modules.
#'
#' @param data A table.
#' @param idcol Name of ID column, defaulting to "ID".
#' @param fail Optional, message to return if ID check fails.
#' @param aschar Convert ID column to character.
#' @export
checkForID <- function(data, idcol = "ID", fail = "<strong>There is no ID column.</strong><br>", aschar = T) {
  hasID <- idcol %in% names(data)
  message <- ifelse(hasID, "", fail)
  if(length(message) && aschar) data[[idcol]] <- as.character(data[[idcol]])
  return(list(message = message, result = data))
}
