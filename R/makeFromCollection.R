#' Create master data table from a collection of datasets
#'
#' Builds one large master dataset given the directory where a collection of datasets resides.
#'
#' This method helps compile the \code{cdata} data object from a collection of datasets.
#' Each dataset represents data from a source/study in the format of a uniquely named .csv/.tsv/.txt file
#' within the specified directory. The files are read and merged together into one master data table.
#' Because IDs for data across all datasets must be unique in the final master data table,
#' namespaced IDs are created using the parent file name. A function can be passed into
#' \code{index} for some control of this namespace index approach. For instance, instead of using the
#' full file name, it might make more sense to map it to a shorter form or to an external key
#' (as long as unique IDs can still be ensured) for namespacing, such that a data feature "Var1"
#' from file "PMID123456_Doe-2000.txt" is identified as "Doe00_Var1" in the master data table.
#'
#' @param cdir Directory path (relative to working directory) to the collection of dataset files.
#' @param filepattern Pattern for grep to identify qualifying files in the directory, e.g. "*.txt".
#' @param exclude A \code{grep} pattern to exclude columns within a file from the final master dataset.
#' @param index Optional, a function to help with namespacing. If not given, defaults to using file name. See details.
#' @return A "master" data.table
#' @export
makeFromCollection <- function(cdir = getwd(), filepattern = "*.txt", exclude = "!", index = NULL) {
  files <- list.files(cdir, pattern = filepattern)
  cdata <- lapply(paste0(cdir, files), function(x) fread(x))
  iref <- if(!is.null(index) & is.function(index)) index(files) else files
  # problem if not unique
  stopifnot(is.unique(iref))
  for(i in seq_along(cdata)) setnames(cdata[[i]], c("ID", paste0(iref[i], "_", names(cdata[[i]])[-1])))
  cdata <- rbindlist(cdata, use.names = T, fill = T)
  cdata <- cdata[, lapply(.SD, Agg), by = ID]
  cdata <- Filter(is.numeric, cdata) # subset columns that actually have data, i.e. is numeric
  cdata <- cdata[, grep(exclude, names(cdata), invert = T), with = F] # omit "!" columns
  return(cdata)
}

#' Make data object for Shiny app
#'
#' A wrapper to create the \code{cdata} object
#'
#' @inheritParams makeFromCollection
#' @param other Optional, a list of of paths to other data files not in the main collection but
#' that should be part of the master data object. NULL if no other data to be incorporated.
#' @param outdir Where to put \code{cdata}. Defaults to App/Data.
#' @export
makeCdata <- function(cdir = "./Collection/Main/", filepattern,
                      other = list("./Collection/Other/coredata_ref.txt"),
                      outdir = "./App/Data/") {
  cdata <- makeFromCollection(cdir, filepattern, exclude)
  if(length(other)) cdata <- Reduce(mergeMore, other, cdata)
  save(cdata, file = paste0(outdir, "cdata.Rdata"))
}


#' Merge a main data table with other misc data
#'
#' Use \code{mergeMore} to merge some other data with the master dataset.
#'
#' @param data A data.table
#' @param inputpath Path to another dataset to merge with data.
#' @return A data.table
mergeMore <- function(data, inputpath) {
  moredata <- fread(inputpath)
  data <- merge(data, moredata, by = "ID", all = T)
  return(data)
}

#' Check metadata for \preformatted{cdata}
#'
#' Checks that data has available matching metadata.
#' @param cdata A data.table, typically output of cdataMake.
#' @param metadata File containing metadata.
#' @return A summary that includes which features are missing what type of metadata.
#' @export
checkMeta <- function(cdata, metadata) {

}


# Adds method to metadata using method key
updateMethod <- function() {
  metadata <- fread("Metadata.tsv")
  methods <- fread("Methods.tsv")
  metadata <- merge(metadata, methods[, .(MethodID, OBITerm)], by = "MethodID")
}

# create a mapping of cases to variable for easy lookup
cases2Variable <- function() {
  c2v <- split(!apply(cdata[, -1], 1, is.na), 1:nrow(cdata))
  c2v <- lapply(c2v, function(x) names(cdata)[-1][x])
  names(c2v) <- cdata$ID
  save(c2v, file = "c2v.rda")
}

# For combining rows, e.g.
# ---------------------------------------------
#ID   var1 var2   var3  var4   var5
#001      0.313      0.344   NA     NA     NA
#001         NA         NA 22.4 41.111 20.669
# becomes -->
#ID var1 var2   var3  var4   var5
#001      0.313      0.344 22.4 41.111 20.669
Agg <- function(x) {
  result <- unique(x[!is.na(x)])
  # Note: returning first(x) is better than return(NA)
  # because it will be the right type of NA for the column
  if(!length(result)) return(data.table::first(x)) else return(result)
}


#  "PMID20062967_Gianani-2010.txt" -> "Gianani10"
indexDefault <- function(files) {
  sapply(strsplit(files, "_|-|\\."), function(x) paste0(x[2], substr(x[3], nchar(x[3])-1, nchar(x[3]))))
}
