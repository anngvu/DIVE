#' Exclude values in dataset #2 that are present in dataset #1
#'
#' Return a version of dataset #2 that doesn't contain records in dataset #1,
#' assuming records are in an "ID" column
#' @keywords internal
excludePresent <- function(x = "ID", data1 = NULL, data2 = NULL) {
  if(is.null(data1)) return(data2)
  data2[!which(data2[[x]] %in% data1[[x]]), ]
}


#' Exclude names referring to ID in vector
#'
#' Used with \code{names}, this filters out the assumed ID ("ID") and returns
#' a version used for downstream subsetting
#' @keywords internal
removeID <- function(x, ID = "ID") Filter(function(f) !f %in% ID, x)
