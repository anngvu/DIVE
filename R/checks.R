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


checkForID <- function(data, message = NULL, result = NULL) {
  hasID <- "ID" %in% names(data)
  if(!hasID) message <- "<strong>There is no ID column.</strong><br>"
  if(is.null(message)) result <- data[, ID := as.character(ID)]
  return(list(message = message, result = result))
}