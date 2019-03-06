#' Hybrid parallel coordinates graph
#' 
#' Visualize a dataset containing both ordinal and categorical variables with 
#' a custom graph built using plotly, and which can be enabled for interactivity
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
#' @param colorBy Name of the variable in data for mapping group colors
#' @param orderAxes A vector giving the order of the axes
#' @param labelAxes Optional, pretty names to use as the axes labels.
#' @param autoScale Logical flag to indicate that numeric variables should be scaled. Defaults to TRUE since this makes
#' the best visual use of space. 
#' @param hideNA A vector giving the factor variable names for which \preformatted{NA} nodes should not be constructed.
#' By default, all factor variables with \preformatted{NA} values will display a \preformatted{NA} node.
#' @param yNAnode Optional, custom axis position for NA node. 
#' @export
hybridParCoordsGraph <- function(data, IDcol = "ID", colorBy, includeColorBy = F, orderAxes, labelAxes, autoScale = T, hideNA, yNAnode = -30) {
  # check columns and automatically factor any character columns not IDcol
  for(col in which(sapply(data, class) == "character" & names(data) != IDcol)) { 
    data[[col]] <- factor(data[[col]]) 
  }
  # for numeric columns other than IDcol, scale to plot scale
  data_scaled <- copy(data)
  vnumeric <- names(data)[which(sapply(data, class) %in% c("numeric", "integer") & names(data) != IDcol)]
  for(i in vnumeric) {
    data_scaled[[i]] <- scales:::rescale(data_scaled[[i]], to = c(0, 100))
  }
  # for factor variables, scale to node plot positions on the graph)
  vcategorical <- names(data)[which(sapply(data, class) == "factor" & names(data) != IDcol)]
  for(i in vcategorical) {
    data_scaled[[i]] <- scales:::rescale(as.numeric(data_scaled[[i]]), to = c(0, 100))
  }
  # long format
  ref <- melt(data, id.vars = IDcol)
  data_scaled <- melt(data_scaled, id.vars = IDcol, value.name = "value_scaled")
  data_scaled[, value_text := ref$value]
  data_scaled[, color := rep(data[[colorBy]], (length(data) - 1)) ]
  setnames(data_scaled, IDcol, "ID")
  nodes <- hybridNodes(data_scaled, vcategorical, yNAnode)
  data_scaled[is.na(value_scaled), value_scaled := yNAnode]
  return(list(data = data_scaled, ID = IDcol, nodes = nodes, vnumeric = vnumeric, vcategorical = vcategorical))
}


hybridNodes <- function(data, categorical, yNAnode) {
  nodes <- list()
  nodes$NAnodes <- data[, .(y = yNAnode, count = sum(is.na(value_scaled))), by = variable]
  # iterate through categorical variables
  for(v in categorical) {
    nodes[[v]] <- data[variable == v, 
                       .(x = unique(variable), y = unique(value_scaled), count = length(value_scaled)), 
                       by = value_text][!is.na(value_text)][order(y)]
  }
  return(nodes)
}
  
colors <-c("Autoab Pos" = "orange", "Cystic fibrosis" = "aquamarine4", "Gastric Bypass" = "bisque4", 
          "Gestational diabetes" = "deeppink2", "Monogenic Diabetes" = "red4", "No diabetes" = "royalblue2", 
          "Other-Diabetes" = "indianred4", "Other-No Diabetes" = "steelblue2", "T1D" = "red",  
           "T1D Medalist" = "maroon", "T2D" = "purple", "Pending" = "gray", "Pregnancy" = "pink", "Transplant" = "darkseagreen4")

#' Construct a hybrid parallel coordinates graph
#' 
#' Visualize a dataset containing both ordinal and categorical variables with 
#' a custom graph built using plotly, and which can be enabled for interactivity
#'
#' @param hpcg Data from \code{\link{hybridParCoordsGraph}}.
#' @param colors Color mappings.
makeHPCG <- function(hpcg, colors) {
  line <- list(type = "line", line = list(color = "black"), xref = "x", yref = "y")
  ax <- list(title = "", zeroline = FALSE, showline = FALSE, showgrid = FALSE)
  lines <- list()
  # set the x-axis label and y-axis limits
  for(v in hpcg$vnumeric) {
    line[c("x0", "x1")] <- v
    line[["y0"]] <- hpcg$data[variable == v, min(value_scaled[value_scaled > 0], na.rm = T)] 
    line[["y1"]] <- hpcg$data[variable == v, max(value_scaled, na.rm = T)] 
    lines <- c(lines, list(line))
  }
  HPCG <- hpcg$data %>% group_by(ID) %>%
    plot_ly(x = ~variable, y = ~value_scaled, hoverinfo = "value_text", height = 500, colors = colors) %>%
    add_lines(color = ~color, alpha = 0.3) %>%
    layout(xaxis = ax, yaxis = c(ax, showticklabels = F), shapes = lines, plot_bgcolor = "white")
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
                  marker = list(sizeref = 0.2),
                  showlegend = F)
  }
  HPCG
}