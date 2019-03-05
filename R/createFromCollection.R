#' Create master data table from a collection of datasets
#'
#' \code{createFromCollection} builds one large master dataset given the
#' directory where a collection of curated datasets resides. This data object is required for
#' the Shiny application.
#'
#' @param cdir Directory path (relative to working directory) to the collection of dataset files.
#' @param filepattern Pattern for grep to identify qualifying files in the directory, e.g. "*.txt".
#' @param exclude A pattern for grep to exclude columns within a file from the final master dataset.
#' @param index Optional function for generating a custom index format from file names,
#' especially useful if naming scheme of the collection files changes.
#' @return A "master" data.table
#' @export
createFromCollection <- function(cdir, filepattern, exclude, index) {
  files <- list.files(path = cdir, pattern = filepattern)
  cdata <- lapply(paste0(cdir, files), function(x) fread(x))
  if(!is.null(index) & is.function(index)) {
    iref <- index(files)
  } else {
    iref <- indexFormat(files)
  }
  for(i in seq_along(cdata)) setnames(cdata[[i]], c("ID", paste0(iref[i], "_", names(cdata[[i]])[-1])))
  cdata <- rbindlist(cdata, use.names = T, fill = T)
  cdata <- cdata[, lapply(.SD, Agg), by = ID]
  cdata <- Filter(is.numeric, cdata) # subset columns that actually have data, i.e. is numeric
  cdata <- cdata[, grep(exclude, names(cdata), invert = T), with = F] # omit "!" columns
  return(cdata)
}

#' Merge a main data table with other misc data
#'
#' Use \code{mergeMore} to merge some other data with the master dataset.
#'
#' @param data A data.table
#' @param inputpath Path to another dataset to merge with data.
#' @return A data.table
#' @export
mergeMore <- function(data, inputpath) {
  moredata <- fread(inputpath)
  data <- merge(data, moredata, by = "ID", all = T)
  return(data)
}

#' Make data object for Shiny app
#'
#' \code{cdataMake} is a wrapper to create the \code{cdata} object
#'
#' @inheritParams createFromCollection
#' @param other A list of of paths to other data files not in the main collection but
#' that should be part of the master data object; an empty list or "" if no other data to be incorporated.
#' @param outdir Where to put \preformatted{cdata}. Defaults to App/Data.
#' @export
cdataMake <- function(cdir = "./Collection/Main/", filepattern = "*.txt", exclude = "!",
                       other = list("./Collection/Other/coredata_ref.txt"),
                       outdir = "./App/Data/") {
  cdata <- createFromCollection(cdir, filepattern, exclude)
  if(length(other)) cdata <- Reduce(mergeMore, other, cdata)
  save(cdata, file = paste0(outdir, "cdata.Rdata"))
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

# Non-exported function for combining rows, e.g.
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

# Each data feature is indexed to the source file;
# an index key is created from the file name, so need to specify the key format.
# e.g. "PMID20062967_Gianani-2010.txt" -> "Gianani10"
indexFormat <- function(files) {
  sapply(strsplit(files, "_|-|\\."), function(x) paste0(x[2], substr(x[3], nchar(x[3])-1, nchar(x[3]))))
}
