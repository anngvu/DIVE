#' Make igraph showing connections between datasets/sources
#'
#' The method first calculates node and edge counts from the collection of datasets.
#' Each dataset/source is a node whose size is proportional to the number of features (columns).
#' More concretely, when each dataset file represents a published study,
#' node size is proportional to the amount of data the study generated
#' (currently, the log of the number of features in the dataset).
#' Edge weights between datasets are counted as the number of overlapping IDs.
#'
#' @param cdir Directory (relative to current working directory) to the collection of dataset files.
#' @param filepattern Pattern to help select files within the directory. Defaults to selecting all files with extension .txt|.tsv|.csv.
#' @param nodes Pattern for extracting node names from source file name. Defaults to using the file name without the extension.
#' @param edges Column within the dataset used for calculating edges,defaults to "ID".
#' @return List containing igraph object and data frame summarizing nodes and edges.
updateNetworkGraph <- function(cdir, filepattern = "*[.](txt|tsv|csv)$", nodes = "([^\\.]*)", edges = "ID") {
  sources <- grep(filepattern, list.files(cdir), value = T)
  names(sources) <- regmatches(sources, regexpr(nodes, sources, perl = T))
  nodes <- lapply(sources, function(x) fread(paste0(cdir, "/",  x), nrows = 0))
  # Check for edge column and warn for any without edge column
  noedge <- sapply(nodes, function(x) !any(grepl(edges, names(x))))
  nodes <- nodes[!noedge]
  if(any(noedge)) warning(paste("removed datasets with missing edge column:", paste(sources[noedge], collapse = ", ")))
  # Check if dataset ONLY contains edge column
  size0 <- lengths(nodes) == 1
  nodes <- nodes[!size0]
  if(any(size0)) warning(paste("removed datasets with no real data:", paste(sources[size0], collapse = ", ")))
  nodesize <- log(lengths(nodes)-1) + 1
  edgewt <- lapply(sources, function(x) fread(paste0(cdir, "/",  x), select = edges)[[1]])
  edgewt <- lapply(edgewt, function(x) sapply(edgewt, function(y) length(intersect(x, y))))
  names(edgewt) <- names(nodes)
  adjm <- as.matrix(do.call(rbind, edgewt))
  network <- igraph::graph_from_adjacency_matrix(adjm, mode = "undirected", weighted = T, diag = F)
  aes <- data.frame(node = names(nodes), size = nodesize)
  return(list(network, aes))
}

#' Modify graph aesthetics based on metadata
#'
#' Change or add node size, color, or other values depending on metadata.
#'
#' @param aes Data frame containing nodes for which aesthetics are added or modified.
#' @param metadata File containing metadata.
#' @param scol Column used for matching the source IDs (nodes), defaults to "Source".
#' @param feature Which aesthetic feature, defaults to "color".
#' @param fcol Column used for mapping to the node feature.
#' @param mapfun Function for mapping values to the feature.
#' @return A data frame containing aesthetics data for the nodes.
graphModAes <- function(aes, metadata, scol = "Source",
                        feature = "color", fcol, mapfun) {
  metadata <- fread(metadata, select = c(scol, fcol))
  values <- metadata[, lapply(.SD, mapfun), by = scol, .SDcols = fcol]
  if(anyNA(values)) warning("Metadata does not match")
  aes[[feature]] <- match(aes$node, values)
}

#' Wrapper to update network graph
#'
#' Apply specified aesthetics to igraph object and exports as
#' interactive visNetwork object for embedding in HTML/Shiny application.
#'
#' @param network An igraph object.
#' @param aes Data frame containing nodes for which aesthetics are added or modified.
#' @result A visNetwork object.
visNetworkMake <- function(network, aes) {
  # Set attributes
  V(network)$size <- aes$size * 5
  V(network)$color <- aes$color
  Ebin <- cut(E(network)$weight, breaks = 10, labels = F)
  grays <- gray.colors(max(E(network)$weight), start = 0.9, end = .1)
  Ecolors <- grays[E(network)$weight]
  E(network)$color <- Ecolors
  visN <- toVisNetworkData(network)
  visN$edges$width <- sqrt(visN$edges$weight)
  g <- visNetwork(visN$nodes, visN$edges)
  save(g, file = "network.Rdata")
}

# Non-exported function to color the nodes depending on whether individual-level data
# is shared for the study, i.e. the node is "online"
isOnline <- function(x, online = "blue", offline = "dimgray") {
  if(any(x == "Yes")) return(online) else return(offline)
}
