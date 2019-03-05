#' Get a correlation matrix and shared samples for each correlation
#'
#' After filtering the input data, get Spearman r using pairwise complete observations
#' as well as the number of pairwise observations available.
#'
#' @param cdata A data.table
#' @param exclude Optional, pattern of columns to exclude, e.g. columns containing IDs, irrelevant or non-numeric values.
#' @return A list with M, the correlation matrix, and N, the number of samples.
#' @export
data2cor <- function(cdata, exclude = "^ID|_SE$|_SEM$|_SD$") {
  cordata <- Filter(is.numeric, cdata) # remove nominal category variables
  cordata <- cordata[, !grepl(exclude, names(cordata )), with = F] # don't do cor on _SE, etc.
  cordata <- Filter(rem0Var, cordata)
  corM <- cor(cordata, use = "pairwise.complete.obs", method = "spearman")
  corN <- crossprod(apply(as.matrix(cordata), 2, function(x) as.integer(!is.na(x)))) # get n sample size
  return(list(M = corM, N = corN))
}

#' Wrapper to instantiate correlation data for Shiny app
#'
#' @param cdata A data.table
#' @return A list with 1) corM, the correlation matrix, and 2) corN, the number of samples available
#' when r was calculated using pairwise complete observation
#' @export
cordataMake <- function(cdata) {
  cordata <- data2cor(cdata)
  save(cordata, file = "Data/correlations.Rdata")
}

# Non-exported function to use with Filter, removes data with 0 variance
rem0Var <- function(x) sd(na.omit(x)) != 0

