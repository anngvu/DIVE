# Functions to make or validate data objects used by DIVE modules, e.g.
# matchApp: refdata
# multiV: hdlist, cdata
# matrixApp:  cordata (M, N, P), cdata, metadata
# dataHelper: handler, lhdata, rhdata
# dataHelper2: dbcon, handler, lhdata, rhdata

#' Convert data table with ID column to matrix with ID as row names
#'
#' @keywords internal
asExprsMatrix <- function(df) {
  mat <- df[, -1]
  mat <- as.matrix(mat)
  rownames(mat) <- unlist(df[, 1], use.names = F)
  return(mat)
}

#' Wrapper to create hdlist (data object for multiV module)
#'
#' Transforms data and annotation files to \code{xVExprs} class objects
#' which are passed in as `hdlist` to \code{\link{multiVServer}} module,
#'
#' This reads an index yaml configuration file that
#' stores files and attributes for data that
#' should be converted to \code{\link{multiVServer}} application data;
#' \code{DIVE::xVExprs} is called to do the conversion.
#'
#' @param index A data.frame containing fields
#' "Title", "Ref", "Type", "DataPath", and "AnnotationPath",
#' where "DataPath" contains path to the data and "AnnotationPath" contains
#' path to the annotation file; other metadata are ignored.
#' @param indexfile Path to a csv (index.csv) or yaml (index.yml)
#' configuration file with the necessary fields described in `index`.
#' @export
hdlistMake <- function(index, indexfile) {
  if(is.null(index)) {
    ext <- tools::file_ext(indexfile)
    if(ext == "csv") {
      index <- data.table::fread(indexfile)
    } else if(ext == "yml") {
      index <- yaml::read_yaml(indexfile)
      index <- data.table::rbindlist(index, fill = T)
    } else{
      error("Expect `indexfile` to be .csv or .yml file.")
    }
  }
  datapaths <- index$DataPath
  annopaths <- index$AnnotationPath
  datalist <- lapply(datapaths, data.table::fread)
  annolist <- lapply(annopaths, readLines)
  titles <- index$Title
  types <-  index$Type
  refs <- index$Ref
  # annotation lines should match number of features (ignoring ID column)
  stopifnot(lengths(datalist)-1  == lengths(annolist))
  datalist <- lapply(datalist, asExprsMatrix)
  hdlist <- purrr::pmap(list(datalist, titles, annolist, types), DIVE::xVExprs)
  names(hdlist) <- paste0(titles, " (", refs, ")")
  return(hdlist)
}

#' Make custom selection from final hdlist
#'
#' This should follow \code{\link{hdlistMake}} since
#' \code{\link{multiVServer}} requires passing in a list object for `choices`.
#'
#' @param hdlist Output of \code{\link{hdlistMake}}.
#' @export
hdlistchoicesMake <- function(hdlist) {
  split(names(hdlist), sapply(hdlist, attr, "type"))
}


#' Create master data table from a collection of datasets
#'
#' Builds one large master dataset given the directory where a dataset collection lives
#'
#' This compiles the \code{cdata} data object from a collection of datasets.
#' Each dataset is a uniquely named .csv|.tsv|.txt file within the specified directory.
#' The files are read and merged together into one master \code{data.table}.
#' Because column IDs must be unique in the table, namespaced IDs are created using the parent file name.
#' A function can be passed into \code{indexfun} for some control of this namespaced index approach.
#' For instance, instead of using the full file name, one might need to map it to a shorter key,
#' pre-existing uuid, or other external key (as long as unique IDs can still be ensured),
#' e.g. a data feature "Var1" from file "PMID123456_Doe-2000.txt" is column named "Doe00_Var1"
#' in the master data table.
#'
#' @param datadir Path to the collection of dataset files.
#' @param files Character vector of file names to select in `datadir`.
#' If NULL, uses `filepattern` to select files.
#' @param filepattern Pattern for grep to identify qualifying files in `datadir`.
#' Ignored if files are already specified.
#' @param keyname Tables are merged using this key column.
#' @param filterfun Optional, a filter function that returns selected columns within a file
#' to be included in the final master dataset, such as to include only numeric columns.
#' @param namespacefun Optional, a function to make unique namespaces.
#' If not given, defaults to namespacing using filenames. See details.
#' @return A "master" \code{data.table}
#' @export
cdataMake <- function(datadir,
                      files = NULL,
                      filepattern = "*",
                      keyname = "ID",
                      filterfun = NULL,
                      namespacefun = defaultIndex) {
  if(is.null(files)) files <- list.files(datadir, pattern = filepattern, full.names = TRUE)
  cdata <- lapply(files, function(x) data.table::fread(x))
  # apply filterfun
  if(!is.null(filterfun) && is.function(filterfun)) {
    cdata <- lapply(cdata, function(dt) filterfun(dt))
  }
  # apply namespacefun
  ID <- NULL # avoid NOTE from non-standard evaluation
  namespaces <- if(!is.null(namespacefun) && is.function(namespacefun)) {
    namespacefun(basename(files))
    # TO DO: check that custom namespacefun indeed generated unique namespaces
    } else {
      tools::file_path_sans_ext(basename(files))
    }
  for(i in seq_along(cdata)) {
    setnames(cdata[[i]], c(keyname, paste0(namespaces[i], "_", names(cdata[[i]])[-1])))
  }
  cdata <- rbindlist(cdata, use.names = T, fill = T)
  cdata <- cdata[, lapply(.SD, colAgg), by = keyname]
  return(cdata)
}

#' Simple wrapper to make cordata
#'
#' @param cdata Output from \code{\link{cdataMake}}.
#' @export
cordataMake <- function(cdata) {
  data2cor(cdata)
}


#' Validate refdata object used in matchApp
#'
#' The refdata object is required in \code{\link{matchAppServer}}.
#' For now, this calls an internal function to check that whatever is used
#' as refdata contains numeric/factor-encoded attributes only,
#' except for the fields given by customdata and subsetv.
#' @param dataset A `data.table`.
#' @param customdata Attribute column name, defaults to "Cohort".
#' This should be changed if matching on something else, e.g. "Sites".
#' @param subsetv Optional, the name of the column storing subset attribute
#' for subsetted matching.
#' @export
validateRefData <- function(dataset, customdata = "Cohort", subsetv = NULL) {
  dataset <- data.table::as.data.table(dataset)
  validcustom <- customdata %in% names(dataset)
  dataset2 <- dataset[, !names(dataset) %in% c(customdata, subsetv), with = F]
  result <- checkCohortData(dataset2)
  if(is.null(result$message) && validcustom) TRUE else FALSE
}


# Utils -------------------------------------------------------------------------------#

#' Coalesce-like function for combining rows
#'
#' Example:
#' ID   var1 var2   var3  var4   var5
#' 001      0.313      0.344   NA     NA     NA
#' 001         NA         NA 22.4 41.111 20.669
#' becomes -->
#' ID var1 var2   var3  var4   var5
#' 001      0.313      0.344 22.4 41.111 20.669
#' @param x A `data.table`.
#' @keywords internal
colAgg <- function(x) {
  result <- unique(x[!is.na(x)])
  # Note: returning first(x) is better than return(NA) bc it ensure right type of NA for the column
  if(!length(result)) return(data.table::first(x)) else return(result)
}

#' Default index maker
#'
#' Create index using file names, e.g. "PMID20062967_Gianani-2010.txt" -> "Gianani10"
#' @param filenames Character vector of names.
#' @keywords internal
defaultIndex <- function(filenames) {
  sapply(strsplit(filenames, "_|-|\\."), function(x) paste0(x[2], substr(x[3], nchar(x[3])-1, nchar(x[3]))))
}

