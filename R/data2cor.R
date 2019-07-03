#' Get a heterogeneous correlation/association matrix and observations used
#'
#' The correlation/association matrix is meant for exploratory data analysis and includes
#' correlations computed with Pearson for interval vs interval variables,
#' Spearman's rank correlation for ordinal vs ordinal variables,
#' and Cramer's V for nomimal variables.
#'
#' This implementation first converts data.table into a cleaned matrix, discarding any non-numeric data
#' and excluding certain columns unsuitable for correlation calculations (such as "ID" and SE/SD columns by default).
#' Columns to be exluded can be specified through name patterns.
#' Then either Pearson or Spearman correlation is calculated using \code{\link[Hmisc]{rcorr}}.
#' However, for variables which are actually nominal and dummy-encoded, the correlations are replaced with association
#' statistics eta.
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


#' Wrapper to instantiate association data for Shiny app
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

