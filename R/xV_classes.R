# Class definition ---------------------------------------------------------------------------------#

#' @keywords internal
new_xVMatrix <- function(x, class = character(), title = NULL, altj = NULL, ...) {
  stopifnot(is.matrix(x))
  structure(x, class = c(class, "xVMatrix"), title = title, altj = altj, ...)
}

# Provides minimal S3 class implementation for expression matrix data that goes into xV module
# xVExprs should be a subclass of xVMatrix

# Constructor
# Need to store some extra dataset data as attributes:
# 1. Type, e.g. "genomics" vs "proteomics"
# 2. Common-denominator/interoperable annotation/cross-index alongside default annotation for type
# Allowed types may change once xV can handle other types of data
#' @keywords internal
new_xVExprs <- function(x, title = NULL, altj = NULL, type = "transcriptomics") {
  type <- match.arg(type, c("transcriptomics", "proteomics"))
  new_xVMatrix(x, class = "xVExprs", title = title, altj = altj, type = type)
}

# Validator
# Check that x is numeric matrix, has valid row and column names, valid type,
# check cross-index is as same length as names if available


#' Helper to construct dataset object for use in \code{\link{xVServer}} module
#'
#' @param x A numeric matrix that should F features as columns and S samples as rows,
#' with appropriate names/IDs as column names and row names.
#' @param title Character title for the dataset.
#' @param altj A character or integer vector matching length of features (columns),
#' intended to be used as an interoperable alternate index.
#' @param type Either "genomics" or "proteomics", currently.
#' @export
xVExprs <- function(x, title = NULL, altj = NULL, type = "transcriptomics") {
  new_xVExprs(x, title = title, altj = altj, type = type)
}


# Methods ---------------------------------------------------------------------------------------#

subsetAltJ <- function(x, lookup) {
  ind <- attr(x, "altj")
  if(is.null(ind)) return(NULL)
  x[, ind %in% lookup, drop = F]
}

# TO DO
# Convert other data to xVExpr function
# Convert ExpressionSet function
