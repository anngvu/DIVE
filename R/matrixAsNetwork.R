#' Shiny module UI for network graph
#'
#' Create app UI for a network graph
#'
#' This is a module with a visNetwork plot as its main output,
#' meant to implement an alternative view of data as a graph.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param height Height passed to `visNetworkOutput`.
#' @param ... Other attributes or elements to be in container.
#' @export
matrixAsNetworkUI <- function(id, height = "400px", ...) {
  ns <- NS(id)
  tags$div(class = "network-ui", id = ns("network-ui"), ... ,
           conditionalPanel("!output.network", ns = ns, class = "dive-loader", id = ns("loader"), "loading..."),
           uiOutput(ns("add_nodes_btn")),
           visNetwork::visNetworkOutput(ns("network"), height = height)
  )
}

#' Server module server for network graph
#'
#' Make a network graph representation of matrix data using `visNetwork`
#'
#' This is a module with a visNetwork plot as its main output,
#' meant to implement an alternative view of data as a graph.
#' As the name of the module suggests, that data should be in matrix form;
#' the module can use whatever is returned by the `matrixCtrl` module.
#' Multi-selection of nodes is set as the default interaction,
#' so return is the currently selected nodes as a character vector node IDs
#' (this can be changed to single select).
#'
#' @param id Namespace id for module.
#' @param mdata Reactive matrix data from \code{\link{matrixCtrlServer}}.
#' @param M Full matrix from which `mdata` originates.
#' @param background Optional background color for network graph.
#' @param .nodes Node options passed to \code{visNodes}.
#' @param .edges Edge options passed to \code{visEdges}.
#' @param .options Options passed to \code{visOptions}.
#' @param .interaction Interaction options passed to \code{visInteraction}. The default enables `multiselect`.
#' @param randomSeed A seed number for reproduceable graph layout.
#' @export
matrixAsNetworkServer <- function(id,
                                  mdata, M,
                                  background = NULL,
                                  .nodes = NULL,
                                  .edges = NULL,
                                  .options = NULL,
                                  .interaction = list(multiselect = TRUE),
                                  randomSeed = NULL) {

  moduleServer(id, function(input, output, session) {

    #-- Main graph output ------------------------------------------------------------------------------------------------------#

    output$network <- visNetwork::renderVisNetwork({
      req(mdata$filM)
      m <- mdata$filM
      # cat("nodes: ", length(allnodes), "edges :", nrow(ind))
      gdata <- dtNodesEdges(m)
      graph <- visNetwork::visNetwork(nodes = gdata$nodes, edges = gdata$edges, background = background) %>%
        visNetwork::visEvents(selectNode = sprintf("function(nodes) { Shiny.onInputChange('%s', nodes.nodes); }",
                                                   session$ns("network_selectednodes")),
                  deselectNode = sprintf("function(nodes) { Shiny.onInputChange('%s', nodes.nodes); }",
                                         session$ns("network_selectednodes")))
      graph <- rlang::exec(visNetwork::visNodes, graph = graph, !!!.nodes)
      graph <- rlang::exec(visNetwork::visEdges, graph = graph, !!!.edges)
      graph <- rlang::exec(visNetwork::visOptions, graph = graph, !!!.options)
      graph <- rlang::exec(visNetwork::visInteraction, graph = graph, !!!.interaction)
      graph <- visNetwork::visIgraphLayout(graph, randomSeed = randomSeed)
    })

    #-- Graph interactives -----------------------------------------------------------------------------------------------------#

    # Show add_nodes option only when a node is selected
    output$add_nodes_btn <- renderUI({
      req(input$network_selected)
      absolutePanel(actionButton(session$ns("add_nodes"), "expand selected"), fixed = TRUE, style = "z-index:10;")
    })

    # Create input$network_nodes via visGetNodes
    observeEvent(input$add_nodes, {
      visNetwork::visNetworkProxy(session$ns("network")) %>% visNetwork::visGetNodes()
    })

    # Get and set new (connected) nodes
    observeEvent(input$network_nodes, {

      n.input <- input$network_selected
      n.connected <- M[, n.input]
      n.connected <- which(n.connected > 0)
      if(length(n.connected)) {
        # new edges:
        new_edges <- data.frame(from = n.input, to = n.connected)
        # new nodes -- manually calc coords to arrange new nodes (visIgraphLayout doesn't work w/ NetworkProxy)
        node_data <- input$network_nodes[[n.input]]
        theta <- seq(0, 2*pi, length.out = length(n.connected))
        x <- node_data$x + cos(theta) * 150
        y <- node_data$y + sin(theta) * 150
        new_nodes <- data.frame(id = n.connected, label = names(n.connected), x = x, y = y)

        visNetwork::visNetworkProxy("network") %>%
          visNetwork::visUpdateEdges(new_edges) %>%
          visNetwork::visUpdateNodes(new_nodes) %>%
          visNetwork::visSelectNodes(id = n.input)
      }

    })

    # Return data.tables storing node and edge data, can be used for non-square matrices
    dtNodesEdges <- function(m) {
      m <- abs(m)
      from <- if(!is.null(rownames(m))) rownames(m) else paste0("V", 1:nrow(m))
      m <- as.data.table(m)
      m[, from := from]
      m <- melt(m, id.vars = "from", variable.name = "to", value.name = "edgewt")
      # remove edges where diagonal, NAs, edgewts < threshold value
      m <- m[!is.na(edgewt)][!from == to]
      nodes <- m[, .(id = union(from, to))]
      # define groups for nodes using metadata -- unlike with the matrix display,
      # can really only apply color using one metadata category (group)
      # as there is only one group column that can be specified in the node df
      # nodes[, group := "")]
      edges <- m[, .(from, to)]
      return(list(nodes = nodes, edges = edges))
    }

    #-- Return -----------------------------------------------------------------------------------------------------#

    ss <- reactive({
      input$network_selectednodes
    })

    return(ss)

  })

}
