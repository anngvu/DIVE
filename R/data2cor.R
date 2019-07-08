#' Get a heterogeneous correlation/association matrix and observations used
#'
#' The correlation/association matrix is meant for exploratory data analysis and can generate
#' Pearson or Spearman correlations for interval vs interval,
#' interval vs ordinal, or ordinal vs ordinal variables.
#'
#' This implementation first converts data.table into a cleaned matrix, discarding any non-numeric data
#' and excluding certain columns unsuitable for correlation calculations (such as "ID" and SE/SD columns by default).
#' Columns to be exluded can be specified through name patterns.
#' Then either Pearson or Spearman correlation is calculated using \code{\link[Hmisc]{rcorr}}, and the method
#' can be specified depending on the data input. Because the original use case includes ordinal data
#' as well as data that might violate normality assumptions, Spearman's is set as the default.
#' In the future it might be possible to extend the implementation to include nominal variables by using association statistics
#' such as Cramer's V, eta, lambda (something like data2association) so the result is heterogeneous
#' correlation/association matrix.
#'
#' @param cdata A data.table
#' @param exclude Optional, name pattern of column to exclude, e.g. columns containing IDs or other data where relationships are not calculated.
#' @return A list with M, the correlation matrix, and N, the number of samples matrix.
#' @export
data2cor <- function(cdata, exclude = "^ID$|_SE$|_SEM$|_SD$", method = "spearman") {
  cordata <- Filter(is.numeric, cdata) # remove nominal category variables
  cordata <- cordata[, !grepl(exclude, names(cordata )), with = F] # don't do cor on _SE, etc.
  cordata <- rem0Var(cordata)
  result <- Hmisc::rcorr(as.matrix(cordata), type = method)
  return(list(M = result$r, N = result$n, P = result$P))
}


#' Wrapper to instantiate association/correlation data for Shiny app
#'
#' @param cdata A data.table
#' @return A list with 1) corM, the correlation matrix, and 2) corN, the number of samples available
#' when r was calculated using pairwise complete observation
#' @export
makeAssociation <- function(cdata) {
  cordata <- data2cor(cdata)
  save(cordata, file = "Data/correlations.Rdata")
}

# Convenience function to use with Filter, removes data with 0 variance
rem0Var <- function(dt) Filter(function(x) sd(na.omit(x)) != 0, dt)

