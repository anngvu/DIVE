# Internal build function composed of the sequential steps for building the network graph
makeVisNetworkGraph <- function(adjm = NULL, sizecol = "Dimensions", colorcol = "InApp", labelcol = "Contributor") {
  adjm <- inCommon(namepattern = "([^_]*)")
  nodemods <- nodeMod(nodes = rownames(adjm), metadata = metadata,
                      mfuns = list(vSize, vBinColor_magma, data.table::first),
                      mcols = c(size = sizecol, color = colorcol, label2 = labelcol))
  network <- networkGraph(adjm = adjm, nodemods = nodemods)
  eigenc <- igraph::eigen_centrality(network)
  visNetD <- visNetwork::toVisNetworkData(network)
  visNetD$nodes$label <- NULL
  # visN$nodes$label <- visN$nodes$label2 # label isn't transferred properly automatically
  visNetD$nodes$eigenscore <- round(eigenc$vector, 3)
  # visN$nodes$title <- paste0("<strong>", visN$nodes$label, "</strong><br>Eigenvector centrality score: ", round(eigenc$vector, 3))
  visNetD$edges$width <- visNetD$edges$weight # weight aes is stored as "width" for visNetwork
  return(visNetD)
}

#' Make custom network graph representing relationships between studies or data
#'
#' This is a wrapper around several \code{\link[igraph]} methods to build a custom network graph
#' with annotations representing relationships between experimental studies or data.
#' In the original purpose, the "relationship" represented by the graph's weighted edges
#' is the number of shared samples between each study, but the function is made generic
#' with the idea that other relationships (e.g. correlation) can be represented as well.
#' Edges and weights are set by passing in any appropriate adjacency matrix.
#' After the graph is constructed, node sizes and colors are modified if a
#' \code{data.frame} containing columns with values for those aesthetics is also passed in.
#' While node size is typically used to indicate the amount of data the study generated,
#' color can be used for other annotations. Finally, the resulting object is by default
#' converted into an interactive \code{\link[visNetwork]} object so it can be part of the Shiny application.
#'
#' @param adjm Adjacency matrix encoding relationships between source nodes.
#' @param edgemods An optional function of adjusting weight values in the adjacency matrix.
#' For aesthetic reasons, weights are scaled by sqrt. Set to \code{NULL} if this is unnecessary.
#' @param nodemods Optional, a \code{data.frame} with 'node' column containing node names matching names in adjacency matrix,
#' and 'size' and/or 'color' column(s) specifying aesthetics.
#' @return An \code{\link[igraph]} with aesthetics.
#' @export
networkGraph <- function(adjm, edgemods = NULL, nodemods = NULL) {
  network <- igraph::graph_from_adjacency_matrix(adjm, mode = "undirected", weighted = T, diag = F)
  grays <- gray.colors(max(igraph::E(network)$weight), start = 0.4, end = 1)
  grays <- grays[igraph::E(network)$weight]
  network <- igraph::set_edge_attr(graph = network, name = "color", value = grays)
  igraph::E(network)$weight <- sqrt(igraph::E(network)$weight)
  if(!is.null(nodemods)) {
    for(a in names(nodemods)) network <- igraph::set_vertex_attr(graph = network, name = a, value = nodemods[[a]])
  }
  return(network)
}

#' Generate an adjacency matrix representing some observation in common for multiple sources
#'
#' This is a helper for constructing an adjacency matrix based on shared case samples
#'
#' @param cdir Directory containing collection of source files. Defaults to working directory.
#' @param filepattern Pattern for selecting files to include.
#' @param namepattern Pattern for extracting node names from source file name. Defaults to using the file name without the extension.
#' @param connection Name of column used for calculating common connection; this column should be present in all datasets. Defaults to "ID".
#' @return An adjacency matrix.
#' @export
inCommon <- function(cdir = getwd(), filepattern = "*[.](txt|tsv|csv)$", namepattern = "([^\\.]*)", connection = "ID") {
  sources <- grep(filepattern, list.files(cdir), value = T)
  names(sources) <- regmatches(sources, regexpr(namepattern, sources, perl = T))
  # first, only check header for connection column and warn for any without
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
#' @param metadata A \code{data.table}.
#' @param IDcol Column in metadata table used for matching node names. Defaults to "Source".
#' @param mfuns A list of one or more functions that map values in a column to size, color, or another node feature.
#' @param mcols A named vector of the corresponding column(s) to use as input into \code{mfuns}. Names should correspond to the node feature.
#' @return A \code{data.frame} containing aesthetics data for the nodes.
#' @export
nodeMod <- function(nodes, metadata, IDcol = "Source", mfuns, mcols) {
  inmeta <- nodes %in% metadata[[IDcol]]
  if(!all(inmeta)) stop(paste0("metadata is missing information for nodes:", nodes[!inmeta]))
  attributes <- metadata[get(IDcol) %in% nodes, Map(function(f, x) f(x), mfuns, .SD), by = IDcol, .SDcols = mcols]
  setnames(attributes, c("node", names(mcols)))
  return(attributes)
}

#' To get size, a numeric column is summed by each node, which is then usually log-scaled.
#' Note: For simplicity, NA is substituted by 1. Because sizes are log-scaled
#' and only meant to be proportionally representative, this doesn't matter too much.
#' @param x A numeric vector.
#' @param logscale Whether to log-scale.
vSize <- function(x, logscale = T) {
  x[is.na(x)] <- 1
  size <- sum(x)
  if(logscale) size <- log(size) + 1
  size <- size * 5
  return(size)
}

#' Map a binary vector to a color. For example, the vector \preformatted{c(T, F, F)}
#' could be assigned the color white if any value is true (true is dominant),
#' or black if any value is false (black is dominant),
#' or one could use the proportion of true/false to get some gray color.
#'
#' @param x A logical vector.
#' @param domlogic Dominant logical. Defauls to true.
#' @param shades A vector of colors with at least two colors, where the last color is mapped to the dominant logical.
#' Defaults to "black" and "white". If more than two colors are given, the proportion of the dominant logical in x is used
#' to assign the closest shade.
vBinColor <- function(x, domlogic = T, shades = c("black", "white")) {
  x <- as.logical(x)
  stopifnot(is.logical(x), is.logical(domlogic))
  nshades <- length(shades)
  if(nshades > 2) {
    shadesteps <- seq(0, 1, by = 1/(nshades - 1))
    x <- sum(as.integer(x), na.rm = T)/length(x)
    shade <- shades[which.min(abs(shadesteps - x))]
  } else {
    shade <- ifelse(all(x != domlogic), shades[1], shades[2])
  }
  shade
}

vBinColor_magma <- function(x, n = 10) {
  shades <- viridis::magma(n, begin = 0.5)
  vBinColor(x, shades = shades)
}
