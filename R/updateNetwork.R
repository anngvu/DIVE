#' Make igraph showing connections between investigations
#'
#' This builds a network graph to represent connections between investigations (studies),
#' where node sizes are proportional to the amount of data the study generated and
#' edge weights are proportional to the number of shared samples between each study.
#' (But edge weights can also set to be proportional to other relationship measures, i.e. correlation.)
#' For edge weights, the method can be given an adjacency matrix
#' or a table that can be used to be build an adjacency matrix.
#' For nodes, a table that with a column (Source, Study, etc.) gives the ID for the node,
#' and a numeric column (Dimensions, Counts, Features, etc.) is summed by each Source to give the node size.
#' (Currently, node size is the log of the number of features to deal with studies with high-throughput data.)
#'
#' @param edges Directory containing collection of dataset files. Defaults to working directory.
#' @param nodes Pattern for extracting node names from source file name. Defaults to using the file name without the extension.
#' @return List containing igraph object and data frame summarizing nodes and edges.
#' @export
networkGraph <- function(cdir = getwd(), filepattern = "*[.](txt|tsv|csv)$", nodes = "([^\\.]*)", edges) {
  sources <- grep(filepattern, list.files(cdir), value = T)
  names(sources) <- regmatches(sources, regexpr(nodes, sources, perl = T))
  # First, only check header for edge column and warn for any without edge column
  nodes <- lapply(sources, function(x) fread(paste0(cdir, "/",  x), nrows = 0))
  noedge <- sapply(nodes, function(x) !any(grepl(edges, names(x))))
  nodes <- nodes[!noedge]
  if(any(noedge)) warning(paste("removed datasets with missing edge column:", paste(sources[noedge], collapse = ", ")))
  # Check if dataset ONLY contains edge column
  size0 <- lengths(nodes) == 1
  nodes <- nodes[!size0]
  if(any(size0)) warning(paste("removed datasets with no data:", paste(sources[size0], collapse = ", ")))
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
#' @param aes Data frame containing nodes to which aesthetics are added or modified.
#' @param metadata Either a path to a metadata file or a data.table already in the environment.
#' @param sourcecol Column used for matching node names, defaults to "Source".
#' @param feature An aesthetic feature, defaults to "color".
#' @param featurecol Column used for mapping to the node feature.
#' @param mapfun Function that maps values in the feature column to the desired feature labels.
#' @return A data frame containing aesthetics data for the nodes.
graphModAes <- function(aes, metadata, sourcecol = "Source", feature = "color", featurecol, mapfun) {
  if(!match("data.table", class(metadata))) {
    if(file.exists(metadata)) metadata <- fread(metadata, select = c(sourcecol, featurecol))
  }
  #
  values <- metadata[, lapply(.SD, mapfun), by = sourcecol, .SDcols = featurecol]
  if(anyNA(values)) warning("Metadata does not match")
  aes[[feature]] <- match(aes$node, values)
  aes
}

#' Wrapper to update network graph
#'
#' Apply specified aesthetics to igraph object and exports as
#' interactive visNetwork object for embedding in HTML/Shiny application.
#'
#' @param network An igraph object.
#' @param aes Data frame containing nodes for which aesthetics are added or modified.
#' @return A visNetwork object.
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

updateNetworkWrapper <- function(cdir = system.file("cdata/curated", package = "DIVE"), nodes = "([^\\_]*)", edges = "ID") {
  networkGraph(cdir = cdir, nodes = nodes, edges = edges)
}
