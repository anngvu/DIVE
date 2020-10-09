#' Shiny module UI for interactive matrix with drilldown component
#'
#' Creates app UI for a heatmap matrix responding to reactive data that is linked to a drilldown scatterplot component.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param ... Other attributes and child elements to pass into UI container.
#' @export
matrixMainUI <- function(id, ...) {
  ns <- NS(id)
  tags$div(class = "matrix-ui", id = ns("matrix-ui"), ... ,
           conditionalPanel("!output.main", ns = ns, class = "dive-loader", id = ns("loader"), "loading..."),
           div(class = "matrix-options", uiOutput(ns("palettes"))),
           div(plotlyOutput(ns("main")))
  )
}

#' Server module server for matrix with drilldown interaction
#'
#' The matrix responds to reactive plot data and has a linked drilldown component.
#'
#' The module handles interactive display of a matrix linked to a scatterplot
#' that accesses underlying data for a user-clicked cell.
#' It works with \code{\link{matrixCtrlServer}}, which returns the data object.
#' Generally, this module only handles visualization of a matrix
#' while the actual matrix generation can be handled by other modules that feed into it.
#' For the drilldown component that displays underlying data points,
#' the statistical data type (numeric vs. ordinal) matters and may require
#' specifying aesthetic parameters for best representation and consistency across modules.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param mdata Reactive matrix data from \code{\link{matrixCtrlServer}}.
#' @param colorscales Optional, a list of custom colorscale functions that takes a numeric matrix and returns either a named coloscale
#' or custom colorscale used for heatmap. If not given, two default colorscale functions are used.
#' @export
matrixMainServer <- function(id,
                             mdata,
                             colorscales = list(default = list(colorscale_named(pal = "RdBu"), zmin = -1, zmax = 1),
                                             absolute = list(colorscale_heatmap_absolute, zmin = -1, zmax = 1))
                            ) {

  moduleServer(id, function(input, output, session) {

    output$palettes <- renderUI({
      tags$div(radioButtons(session$ns("colorscale"), label = NULL, choices = names(colorscales), inline = TRUE),
               title = "Select the color mapping for data")
    })

    # Generate meta marginal plots
    metamargin <- function(x, y, z, name = "Group", text, colorscale = "Portland") {
      axis <- list(title = "", showgrid = F, showticklabels = F, ticks = "")
      plotly::plot_ly(type = "heatmap", x = x, y = y, z = z,
              name = name, text = text, hovertemplate = "<b>%{text}</b>",
              showscale = FALSE, colorscale = colorscale) %>%
        plotly::layout(xaxis = axis, yaxis = axis)
    }

    #-- Main matrix plot -----------------------------------------------------------------------------------------------------#

    matrixheatmap <- reactive({
      req(input$colorscale)
      M <- mdata$filM
      if(is.null(M)) {
        plotly_empty()
      } else if(nrow(M) == 0 || ncol(M) == 0) {
        plotly_empty() %>% plotly::layout(title = "no result with selected filters")
      } else {
        # bug? plot not displayed if height is less than 100px; minpx = 5
        px <- 1200/ncol(M)
        height <- nrow(M) * px
        height <- if(height < 400) 400 else height
        colorz <-  colorscales[[input$colorscale]][[1]](M)
        axis <- list(title = "", showgrid = F, automargin = TRUE, showticklabels = nrow(M) <= 30, # show labels when not too crowded
                     ticks = "", tickfont = list(color = "gray"), linecolor = "gray", mirror = T)

        plotly::plot_ly(type = "heatmap", x = colnames(M), y = rownames(M), z = M, name = "Exploratory\nMap",
                     colorscale = colorz, zmin = colorscales[[input$colorscale]]$zmin, zmax = colorscales[[input$colorscale]]$zmax,
                     hovertemplate = "row: <b>%{y}</b><br>col: <b>%{x}</b><br>corr: <b>%{z}</b>",
                     height = height, colorbar = list(thickness = 8)) %>%
          plotly::layout(xaxis = axis, yaxis = axis, plot_bgcolor = colorscales[[input$colorscale]]$bgcolor) %>%
          event_register("plotly_click")
      }
    })

    # Additional group annotations for rows, shown as a vertical subplot in right margin of p
    rowmeta <- reactive({
      if(is.null(mdata$rowmeta)) return()
      datarownames <- rownames(mdata$filM)
      text <- matrix(paste(mdata$rowmeta))
      metamargin(x = "Group", y = datarownames, z = matrix(as.integer(mdata$rowmeta)),
                text = text, name = "Row Group")
    })

    # Additional group annotations for cols, shown as a horizontal subplot in top margin of p
    colmeta <- reactive({
        if(is.null(mdata$colmeta)) return()
        datacolnames <- colnames(mdata$filM)
        text <- matrix(paste(mdata$colmeta), nrow = 1)
        metamargin(x = datacolnames, y = "Group", z = matrix(as.integer(mdata$colmeta), nrow = 1),
                   text = text, name = "Column Group")
    })

    # Plot compose
    output$main <- renderPlotly({
      # plot_bgcolor can't be independent for individual plots in subplot, adjust layout based on whether metadata exists
      if(is.null(rowmeta()) && is.null(colmeta())) {
        main <- matrixheatmap()
      } else if(is.null(colmeta())) {
        main <- plotly::subplot(matrixheatmap(), rowmeta(), nrows = 1, shareY = T, margin = 0.01, widths = c(0.97, 0.03))
      } else {
        main <- plotly::subplot(colmeta(), plotly_empty(), matrixheatmap(), rowmeta(),
                        nrows = 2, shareX = T, shareY = T, margin = 0.01,
                        widths = c(0.97, 0.03), heights = c(0.03, 0.97))
      }
      main$x$source <- session$ns("main")
      main %>% plotly::config(displayModeBar = F)
    })


    #-- Return -----------------------------------------------------------------------------------------------------#

    ss <- reactive({
      s <- event_data("plotly_click", source = session$ns("main"))
      if(is.null(s)) return()
      v1 <- s[["x"]]
      v2 <- s[["y"]]
      c(v1, v2)
    })

    return(ss)

  })
}

# -- Helpers -----------------------------------------------------------------------------------------------------#

colorscale_named <- function(pal = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdGy", "RdYlBu", "RdYlGn",
                                        "Spectral", "Accent", "Dark2", "Paired",
                                        "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
                                        "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                                        "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
                                        "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")) {
  pal <- match.arg(pal)
  colorfun <- function(z) pal
  return(colorfun)
}

#' Create custom colorscale for `plotly` heatmap
#'
#' Pass in a custom palette to generate custom colorscale data required by `plotly`
#'
#' This simply calls an internal function which relies on `scales::rescale`.
#' @param palette A custom palette.
#' @export
colorscale_heatmap_custom <- function(palette) {
  function(z) colorscale_heatmap_manual(z, palette = palette)
}

# Colorscale function for color mapping values
# Palettes:
# Using a symmetric palette for sign-agnostic coloring focuses only on the magnitude of values
# c("#F3012F", "#404040", "#01F3C5") red-green
colorscale_heatmap_manual <- function(z, domain = c(-1, 1), palette = c("#EF3202", "ghostwhite", "#02BFEF")) {
  z <- c(z)
  z <- z[!is.na(z)]
  z <- unique(scales::rescale(z, domain = domain))
  orderz <- order(z)
  colors <- scales::col_numeric(palette, domain = NULL)(z)
  colorz <- setNames(data.frame(z[orderz], colors[orderz]), NULL)
  return(colorz)
}


colorscale_heatmap_absolute <- function(z) {
  colorscale_heatmap_manual(z, palette = c(grDevices::hcl.colors(10, "YlOrRd", rev = TRUE), grDevices::hcl.colors(10, "YlOrRd")))
}

