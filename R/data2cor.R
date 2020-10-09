#' Get correlation matrix and observations used
#'
#' Get correlation matrix with Pearson or Spearman correlations
#'
#' This implementation first converts data.table into a cleaned matrix,
#' discarding any non-numeric data and excluding certain columns
#' for correlation calculations (such as "ID" and SE/SD columns by default).
#' Columns to be exluded can be specified through name patterns.
#' Pearson or Spearman correlation is calculated using \code{\link[Hmisc]{rcorr}};
#' the method can be specified depending on the data input.
#' Because the original use case includes ordinal data
#' as well as data that might violate normality assumptions,
#' the more conservative Spearman rank correlation is set as the default
#' (interval vs ordinal & ordinal vs ordinal data).
#' In the future it might be possible to extend the implementation
#' to include nominal variables by using other association statistics
#' such as Cramer's V, eta, lambda (something like data2association)
#' so the result will be a heterogeneous correlation/association matrix.
#'
#' @param cdata A `data.table`.
#' @param exclude Optional, name pattern of column to exclude,
#' e.g. columns containing IDs or other data where relationships are not calculated.
#' The default excludes columns named "ID" and columns ending with "_SE", "_SEM", and "_SD".
#' @param type Either "pearson" or "spearman", defaults to "spearman".
#' @return A list with M, the correlation matrix, and
#' N, the number of observations matrix, and
#' P, the p-values matrix. See \code{\link[Hmisc]{rcorr}}.
#' @export
data2cor <- function(cdata, exclude = "^ID$|_SE$|_SEM$|_SD$", type = "spearman") {
  cordata <- Filter(is.numeric, cdata) # remove nominal variables
  cordata <- cordata[, !grepl(exclude, names(cordata )), with = F] # don't do cor on _SE or whatever is specified
  cordata <- rem0Var(cordata)
  result <- Hmisc::rcorr(as.matrix(cordata), type = type)
  return(list(M = result$r, N = result$n, P = result$P))
}


# Convenience function to use with Filter, removes data with 0 variance
rem0Var <- function(dt) Filter(function(x) sd(na.omit(x)) != 0, dt)

