#' Exclude values in dataset #2 that are present in dataset #1
#'
#' @keywords internal
excludePresent <- function(x = "ID", data1, data2) data2[[x]][!which(data2[[x]] %in% data1[[x]]), ]
