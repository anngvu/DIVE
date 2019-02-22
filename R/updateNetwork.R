#' Make graph showing connections between datasets/sources
#'
#' Get node and edge counts from the collection of datasets to build a network graph. Each dataset/source is a node
#' whose size is proportional to the number of features (columns). More concretely, when each dataset file represents
#' a published study, nodes are proportional to the amount of data the study generated. Edge weights between datasets are determined
#' proportional to the number of overlapping IDs.
#'
#' @param cdir Directory path (relative to current working directory) to the collection of dataset files.
#' @param nodes Pattern for extracting node names from source file name, or "" to use the file name.
#' @param edges Column used for calculating edges, usually "ID".
#' @result List containing igraph object and a data frame for graph aesthetics.
networkGraph <- function(cdir, nodes, edges) {
  sources <- grep("*.txt$", list.files(cdir), value = T)
  names(sources) <- regmatches(sources, regexpr(nodes, sources))
  nodes <- lapply(sources, function(x) readLines(paste0(cdir, "/",  x), n = 1))
  nodesz <- log(lengths(nodes)-1) + 1
  edges <- lapply(sources, function(x) fread(paste0(cdir, "/",  x), select = edges))
  edgewt <- lapply(edges, function(x) sapply(edges, function(y) length(intersect(x, y))))
  names(edges) <- names(sources)
  adjm <- as.matrix(do.call(rbind, edges))
  network <- graph_from_adjacency_matrix(adjm, mode = "undirected", weighted = T, diag = F)
  aes <- data.frame(node = names(V(network)), size = nodesz)
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
