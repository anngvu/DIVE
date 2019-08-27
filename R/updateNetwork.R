#' Make custom network graph representing relationships between studies or data
#'
#' This is a wrapper around several \code{\link[igraph]} methods to build a custom network graph
#' with annotations representing relationships between experimental studies or data.
#' In the original purpose, the "relationship" represented by the graph's weighted edges
#' is the number of shared samples between each study, but the function is made generic
#' with the idea that other relationships (e.g. correlation) can be represented as well.
#' Edges and weights are set by passing in any appropriate adjacency matrix.
#' After the graph is constructed, node sizes and colors are modified if a
#' \code{data.frame} containing columns with values for those aesthetics has also been passed in.
#' While node size is typically used to indicate the amount of data the study generated,
#' color can be used for any other annotation. Finally, the resulting object is by default
#' converted into an interactive \code{\link[visNetwork]} object so it can be part of the Shiny application.
#'
#' @param adjm Adjacency matrix encoding relationships between source nodes.
#' @param edgemods An optional function of adjusting weight values in the adjacency matrix.
#' For aesthetic reasons, weights are scaled by sqrt. Set to NULL if this is unnecessary.
#' @param nodemods Optional, a data.frame with 'node' column containing node names matching names in adjacency matrix,
#' and 'size' and/or 'color' column(s) specifying aesthetics.
#' @param asVis Whether to keep as \code{\link[igraph]} or convert to interactive \code{\link[visNetwork]} object for the web (default).
#' @return Either an \code{\link[igraph]} or \code{\link[visNetwork]} object representing desired relationship network.
#' @export
networkGraph <- function(adjm, nodemods = NULL) {
  network <- igraph::graph_from_adjacency_matrix(adjm, mode = "undirected", weighted = T, diag = F)
  grays <- gray.colors(max(E(network)$weight), start = 0.9, end = 0.1)
  grays <- grays[E(network)$weight]
  network <- igraph::set_edge_attr(graph = network, name = "color", value = grays)
  E(network)$weight <- sqrt(E(network)$weight)
  if(!is.null(nodemods)) {
    for(a in names(nodemods)) network <- igraph::set_vertex_attr(graph = network, name = a, value = nodemods[[a]])
  }
  return(network)
}

# Internal build function
makeNetworkGraph <- function(randomSeed = NULL) {
  adjm <- connectByCases(namepattern = "([^_]*)")
  nodes <- rownames(adjm)
  nodemods <- nodeMod(nodes = nodes, metadata = metadata,
                      mfuns = list(vSize, vBinColor, data.table::first), mcols = c(size = "Dimensions", color = "InApp", lab = "Contributor"))
  network <- networkGraph(adjm = adjm, nodemods = nodemods)
  eigenc <- igraph::eigen_centrality(network)
  visN <- visNetwork::toVisNetworkData(network)
  visN$nodes$label <- visN$nodes$lab # label isn't transferred properly automatically
  visN$nodes$title <- paste0("<strong>", visN$nodes$label, "</strong><br>Eigenvector centrality score: ", round(eigenc$vector, 3))
  visN$edges$width <- visN$edges$weight # since weight aes is stored in width for visNetwork
  visN <- visNetwork::visNetwork(visN$nodes, visN$edges)
  return(visN)
}

# Helper for constructing an adjacency matrix based on shared case samples
#' @param cdir Directory containing collection of dataset files. Defaults to working directory.
#' @param filepattern Pattern for selecting files to include.
#' @param namepattern Pattern for extracting node names from source file name. Defaults to using the file name without the extension.
#' @param connection Name of column used for calculating connections; should be present in all datasets. Defaults to "ID".
#' @result An adjacency matrix.
#' @export
connectByCases <- function(cdir = getwd(), filepattern = "*[.](txt|tsv|csv)$", namepattern = "([^\\.]*)", connection = "ID") {
  sources <- grep(filepattern, list.files(cdir), value = T)
  names(sources) <- regmatches(sources, regexpr(namepattern, sources, perl = T))
  # First, only check header for connection column and warn for any without
  nodes <- lapply(sources, function(x) fread(paste0(cdir, "/",  x), nrows = 0))
  noedge <- sapply(nodes, function(x) !any(grepl(connection, names(x))))
  nodes <- nodes[!noedge]
  if(any(noedge)) warning(paste("ignoring datasets with missing connection column:", paste(sources[noedge], collapse = ", ")))
  # Check if dataset ONLY contains connection column (translates to node of size 0)
  size0 <- lengths(nodes) == 1
  nodes <- nodes[!size0]
  if(any(size0)) warning(paste("ignoring datasets with no data:", paste(sources[size0], collapse = ", ")))
  edgewt <- lapply(sources, function(x) fread(paste0(cdir, "/",  x), select = connection)[[1]])
  edgewt <- lapply(edgewt, function(x) sapply(edgewt, function(y) length(intersect(x, y))))
  names(edgewt) <- names(nodes)
  adjm <- as.matrix(do.call(rbind, edgewt))
  return(adjm)
}

#' Modify network graph node aesthetics based on metadata
#'
#' Change or add node size, color, or other annotation values depending on metadata.
#'
#' As asked for by the param \code{IDcol}, in metadata there should be a key column (perhaps called Source, Study, etc.)
#' with node IDs. Meanwhile, \code{mfuns} takes columns whose values can be mapped
#' to node size, color and other node properties through map functions that can be passed into \code{mfuns}.
#'
#' @param nodes Name of nodes to look up in metadata table.
#' @param metadata A data.table.
#' @param IDcol Column used for matching nodes. Defaults to "Source".
#' @param mfuns A list of one or more functions that map values in a column to size, color, or another node feature.
#' @param mcols A named vector of the corresponding column(s) to use as input into mfuns. Names should correspond to the node feature.
#' @return A data.frame containing aesthetics data for the nodes.
#' @export
nodeMod <- function(nodes, metadata, IDcol = "Source", mfuns, mcols) {
  inmeta <- nodes %in% metadata[[IDcol]]
  if(!all(inmeta)) stop(paste0("metadata is missing information for nodes:", nodes[!inmeta]))
  attributes <- metadata[get(IDcol) %in% nodes, Map(function(f, x) f(x), mfuns, .SD), by = IDcol, .SDcols = mcols]
  setnames(attributes, c("node", names(mcols)))
  return(attributes)
}

#' To get size, a numeric column can be summed by each node, which is then usually log-scaled.
#' Note: For simplicity, NA is substituted by 1. Because sizes are log-scaled and only meant to proportionally representative,
#' this doesn't matter too much.
#' @param x A numeric vector.
#' @param scale Whether to log-scale node sizes for aesthetic reasons.
vSize <- function(x, logscale = T) {
  x[is.na(x)] <- 1
  size <- sum(x)
  if(logscale) size <- log(size) + 1
  size <- size * 5
  return(size)
}

#' Map to color based on binary data. Since this takes a logical vector, ncolorscale controls
#' the color mapping to colorT and colorF; use a value of 3 to assign colorT for a proportion = 0.5.
#' @param x A logical vector.
#' @param colorT Color for TRUE.
#' @param colorF Color for FALSE.
#' @param ncolorscale Number of colors to use.
vBinColor <- function(x, colorT = "#2196f3", colorF = "gray20", ncolorscale = 10) {
  x <- sum(as.integer(as.logical(x)), na.rm = T)/length(x)
  value <- cut(x, breaks = seq(0, 1, len = ncolorscale), include.lowest = T, right = F, label = F)
  color <- colorRampPalette(c(colorF, colorT))(ncolorscale-1)[value]
  return(color)
}

