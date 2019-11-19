#' New hybrid parallel coordinates graph object
#'
#' Create data object to be used for visualizing a dataset containing both ordinal and
#' categorical variables with a custom graph built using plotly, and which can be enabled for interactivity
#'
#' Typical parallel coordinates graph work for numeric data, while another version called parallel sets
#' is normally used for categorical data. This "hybrid" graph is an attempt to represent both
#' numeric and categorical data in one graph, and as such it can be described as a typical
#' parallel coordinates graph constructed with floating nodes for categorical (factor) variables.
#' One limitation is that numeric values should be non-negative, because of how \preformatted{NA}
#' is handled in the plotting for the nodes. The nodes correspond to the levels within the variable,
#' and the sizes are proportional to the number of instances within each level.
#' \preformatted{NA} values in factor variables are assigned to a \preformatted{NA} node,
#' unless otherwise specified through \preformatted{hideNA}.
#' The graph is most useful when it is interactive, so it is intended to be embedded as an HTML widget.
#'
#' @param data A data.frame or data.table, in which categorical variables should be factor columns.
#' All character columns except \preformatted{IDcol} will be factored.
#' @param IDcol Name of column containing IDs, or NA if there are no ID columns. Defaults to "ID".
#' @param vcolor Name of the variable in data to be used for mapping to colors.
#' @param vcolorX Logical flag to indicate whether the variable used for \preformatted{vcolor} should be
#' excluded in the graph. Defaults to FALSE.
#' @param orderx Optional, a vector giving custom order of the axes.
#' @param vlabels Optional, character vector for axes labels.
#' @param autoscale Logical flag to indicate that numeric variables should be scaled.
#' Defaults to TRUE since this usually makes the best visual use of space.
#' @param hideNA A vector giving the factor variable names for which
#' \preformatted{NA} nodes should not be constructed. By default,
#' all factor variables with \preformatted{NA} values will display a \preformatted{NA} node.
#' @param yNAnode Optional, custom axis position for NA node.
#' @export
newHPCG <- function(data, IDcol = "ID", vcolor, vcolorX = FALSE,
                    orderx, vlabels, autoscale = T, hideNA, yNAnode = -30) {
  # check columns and automatically factor any character columns not IDcol
  for(col in which(sapply(data, class) == "character" & names(data) != IDcol)) {
    data[[col]] <- factor(data[[col]])
  }
  # for numeric columns other than IDcol, scale to plot scale
  data_scaled <- copy(data)
  if(vcolorX) data_scaled[, (vcolor) := NULL]
  vnum <- names(data_scaled)[which(sapply(data_scaled, class) %in% c("numeric", "integer")
                                   & names(data_scaled) != IDcol)]
  for(i in vnum) {
    data_scaled[[paste0(i, "_scaled")]] <- if(autoscale) scales:::rescale(data_scaled[[i]], to = c(0, 100))
    else data_scaled[[i]]
  }
  # for factor variables, scale to node plot positions on the graph)
  vcat <- names(data_scaled)[which(sapply(data_scaled, class) == "factor"
                                   & names(data_scaled) != IDcol)]
  for(i in vcat) data_scaled[[paste0(i, "_scaled")]] <- scales:::rescale(as.numeric(data_scaled[[i]]), to = c(0, 100))
  # long format
  data_scaled <- melt(data_scaled, id.vars = IDcol,
                      measure = list(value_scaled = paste0(c(vnum, vcat), "_scaled"), value_text = c(vnum, vcat)))
  data_scaled$variable <- c(vnum, vcat)[data_scaled$variable]
  data_scaled[, color := rep_len(data[[vcolor]], length.out = .N) ]
  setnames(data_scaled, IDcol, "ID")
  nodes <- hybridNodes(data_scaled, vcat, yNAnode)
  data_scaled[is.na(value_scaled), value_scaled := yNAnode]
  return(list(data = data, plot_data = data_scaled, ID = IDcol, nodes = nodes,
              vnum = vnum, vcat = vcat, vcolor = vcolor))
}


hybridNodes <- function(data, vcat, yNAnode) {
  nodes <- list()
  nodes$NAnodes <- data[, .(y = yNAnode, count = sum(is.na(value_scaled))), by = variable][count > 0]
  # iterate through categorical variables
  for(v in vcat) {
    nodes[[v]] <- data[variable == v,
                       .(x = unique(variable), y = unique(value_scaled), count = length(value_scaled)),
                       by = value_text][!is.na(value_text)][order(y)]
  }
  return(nodes)
}

#' Construct a hybrid parallel coordinates graph
#'
#' Visualize a dataset containing both ordinal and categorical variables with
#' a custom graph built using plotly, and which can be extended with a linked pie graph
#' in the \code{\link{HPCGraph}} module.
#'
#' @param hpcg Data from \code{\link{hybridParCoordsGraph}}.
#' @param colors Color mappings.
#' @param plotbgcolor Optional plot background color; transparent if not given,
#' @export
plotlyHPCG <- function(hpcg, colors, plotbgcolor = NULL) {
  line <- list(type = "line", line = list(color = "black"), xref = "x", yref = "y")
  ax <- list(title = "", zeroline = FALSE, showline = FALSE, showgrid = FALSE)
  lines <- list()
  # set the x-axis label and y-axis limits
  for(v in hpcg$vnum) {
    line[c("x0", "x1")] <- v
    line[["y0"]] <- hpcg$plot_data[variable == v, min(value_scaled[value_scaled > 0], na.rm = T)]
    line[["y1"]] <- hpcg$plot_data[variable == v, max(value_scaled, na.rm = T)]
    lines <- c(lines, list(line))
  }
  HPCG <- hpcg$plot_data %>% group_by(ID) %>%
    plot_ly(x = ~variable, y = ~value_scaled, hoverinfo = ~value_text, height = 500, colors = colors) %>%
    add_lines(color = ~color, alpha = 0.3) %>%
    layout(xaxis = ax, yaxis = c(ax, showticklabels = F), shapes = lines,
           plot_bgcolor = if(is.null(plotbgcolor)) "rgba(0,0,0,0)" else plotbgcolor,
           paper_bgcolor = if(is.null(plotbgcolor)) "rgba(0,0,0,0)" else plotbgcolor)
  # Add nodes for categorical axes
  HPCG <- HPCG %>% add_trace(data = hpcg$nodes$NAnodes, type = "scatter", mode = "markers",
                             name = "Not applicable/Not available",
                             x = ~variable, y = ~y, size = ~count, key = "NA",
                             text = ~paste(variable, "|", count, "NA"), hoverinfo = "text",
                             marker = list(color = "black", sizeref = 0.2))
  for(node in names(hpcg$nodes)[-1]) {
    HPCG <- HPCG %>% add_trace(data = hpcg$nodes[[node]], type = "scatter", mode = "markers", name = node,
                  x = ~x, y = ~y, size = ~count, key = ~x,
                  text = ~paste(value_text, "| n =", count), hoverinfo = "text",
                  marker = list(color = hpcg$nodes[[node]]$color, sizeref = 0.2),
                  showlegend = F)
  }
  HPCG
}
