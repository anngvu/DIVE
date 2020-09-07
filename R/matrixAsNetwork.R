#' Shiny module UI for network graph
#'
#' Create app UI for a network graph
#'
#' This is a module with a visNetwork plot as its main output,
#' meant to implement an alternative view of data as a graph.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
matrixAsNetworkUI <- function(id, height = "400px", ...) {
  ns <- NS(id)
  tags$div(class = "network-ui", id = ns("network-ui"), ... ,
           conditionalPanel("!output.network", ns = ns, class = "dive-loader", id = ns("loader"), "loading..."),
           uiOutput(ns("add_nodes_btn")),
           visNetworkOutput(ns("network"), height = height)
  )
}

#' Server module server for network graph
#'
#' Making a network graph representation of matrix data using visNetwork.
#'
#' This is a module with a visNetwork plot as its main output,
#' meant to implement an alternative view of data as a graph.
#' As the name of the module suggests, that data should be in matrix form;
#' the module can use whatever is returned by the matrixCtrl module.
#' Multi-selection of nodes is set as the default interaction,
#' so return is the currently selected nodes as a character vector node IDs
#' (this can be changed to single select).
#'
#' @param id Namespace id for module.
#' @param mdata Reactive matrix data from \code{\link{matrixCtrl}}.
#' @param M Full matrix from which `mdata` originates.
#' @param style Optional, a list with global styling for nodes, edges, and plot background. Defaults to visNetwork styling.
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

    output$network <- renderVisNetwork({
      req(mdata$filM)
      m <- mdata$filM
      # cat("nodes: ", length(allnodes), "edges :", nrow(ind))
      gdata <- dtNodesEdges(m)
      graph <- visNetwork(nodes = gdata$nodes, edges = gdata$edges, background = background) %>%
        visEvents(selectNode = sprintf("function(nodes) { Shiny.onInputChange('%s', nodes.nodes); }", session$ns("network_selectednodes")),
                  deselectNode = sprintf("function(nodes) { Shiny.onInputChange('%s', nodes.nodes); }", session$ns("network_selectednodes")))
      graph <- rlang::exec(visNodes, graph = graph, !!!.nodes)
      graph <- rlang::exec(visEdges, graph = graph, !!!.edges)
      graph <- rlang::exec(visOptions, graph = graph, !!!.options)
      graph <- rlang::exec(visInteraction, graph = graph, !!!.interaction)
      graph <- visIgraphLayout(graph, randomSeed = randomSeed)
    })

    #-- Graph interactives -----------------------------------------------------------------------------------------------------#

    # Show add_nodes option only when a node is selected
    output$add_nodes_btn <- renderUI({
      req(input$network_selected)
      absolutePanel(actionButton(session$ns("add_nodes"), "expand selected"), fixed = TRUE, style = "z-index:10;")
    })

    # Create input$network_nodes via visGetNodes
    observeEvent(input$add_nodes, {
      visNetworkProxy(session$ns("network")) %>% visGetNodes()
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

        visNetworkProxy("network") %>%
          visUpdateEdges(new_edges) %>%
          visUpdateNodes(new_nodes) %>%
          visSelectNodes(id = n.input)
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
