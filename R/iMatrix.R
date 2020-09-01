#' Shiny module UI for interactive matrix plot with drilldown components
#'
#' Creates app UI for a heatmap matrix responding to reactive data that is linked to a drilldown scatterplot component.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
iMatrixUI <- function(id) {
  ns <- NS(id)

    tags$div(class = "iMatrixUI",
    tags$div(class = "drilldown-output", id = ns("drilldown-output"),
             tags$script(sprintf('$("#%s").draggable({ start: function(event, ui) {
                                     console.log("moving to:" + event.pageY + " " + event.pageX);
                                     $(this).css({ position: "absolute", top: event.pageY + "px", left: event.pageX + "px"});
                                     window.dispatchEvent(new Event("resize"));
                                    }
                                   });',
                                   ns("drilldown-output"), ns("dock"))),
             selectizeInput(ns("drilldown"), "Drill down to data for", width = "400px",
                            choices = "", selected = "",
                            options = list(maxItems = 2, placeholder = "select variable(s)")),
             conditionalPanel("input.drilldown != ''", ns = ns,
                              div(class = "ui-inline", br(), actionLink(ns("flipxy"), "flip XY", icon = icon("refresh"))),
                              plotlyOutput(ns("scatter")))
            ),
    tags$div(class = "matrix-output", id = ns("matrix-output"),
             conditionalPanel("!output.main", ns = ns, class = "dive-loader", id = ns("loader"), matrixSpinner(), "loading..."),
             div(class = "matrix-options", uiOutput(ns("palettes"))),
             div(plotlyOutput(ns("main")))
    )
  )
}

#' Server module server for plotting correlations matrix with drilldown interaction
#'
#' The matrix responds to reactive plot data and has a linked drilldown component.
#'
#' The module handles interactive display of a matrix linked to a scatterplot
#' that accesses underlying data for a user-clicked cell. It works with \code{\link{matrixCtrl}},
#' which returns the data object. Generally, this module only handles visualization of a matrix
#' while the actual matrix generation can be handled by other modules that feed into it.
#' For the drilldown component that displays underlying data points,
#' the statistical data type (numeric vs. ordinal) matters and may require
#' specifying aesthetic parameters for best representation and consistency across modules.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param mdata Reactive matrix data from \code{\link{matrixCtrl}}.
#' @param factorx A function that returns a boolean for whether a variable should be plotted as factor when given the variable name.
#' @param dcolors Optional, a list with name matching the variable in the data to use for color grouping and custom colors.
#' @param colorscales Optional, a list of custom colorscale functions that takes a numeric matrix and returns either a named coloscale
#' or custom colorscale used for heatmap. If not given, two default colorscale functions are used.
#' @param plotbg Optional, color for matrix plot background.
#' @export
iMatrixServer <- function(id,
                          mdata,
                          factorx = NULL,
                          dcolors = NULL,
                          colorscales = list(default = list(colorscale_named(pal = "RdBu"), zmin = -1, zmax = 1),
                                             absolute = list(colorscale_heatmap_absolute, zmin = -1, zmax = 1))
                          ) {
  moduleServer(id, function(input, output, session) {


    output$palettes <- renderUI({
      tags$div(radioButtons(session$ns("colorscale"), label = NULL, choices = names(colorscales), inline = TRUE),
               title = "Select the color mapping for data")
    })

    #-- Main matrix plot -----------------------------------------------------------------------------------------------------#

    matrixheatmap <- reactive({
      req(input$colorscale)
      M <- mdata$filM
      if(is.null(M)) {
        plotly_empty()
      } else if(nrow(M) == 0 || ncol(M) == 0) {
        plotly_empty() %>% layout(title = "no result with selected filters")
      } else {
        # bug? plot not displayed if height is less than 100px; minpx = 5
        px <- 1200/ncol(M)
        height <- nrow(M) * px
        height <- if(height < 400) 400 else height
        colorz <-  colorscales[[input$colorscale]][[1]](M)
        axis <- list(title = "", showgrid = F, automargin = TRUE, showticklabels = nrow(M) <= 30, # show labels when not too crowded
                     ticks = "", tickfont = list(color = "gray"), linecolor = "gray", mirror = T)

        plot_ly(type = "heatmap", x = colnames(M), y = rownames(M), z = M, name = "Exploratory\nMap",
                     colorscale = colorz, zmin = colorscales[[input$colorscale]]$zmin, zmax = colorscales[[input$colorscale]]$zmax,
                     hovertemplate = "row: <b>%{y}</b><br>col: <b>%{x}</b><br>corr: <b>%{z}</b>",
                     height = height, colorbar = list(thickness = 8)) %>%
          layout(xaxis = axis, yaxis = axis, plot_bgcolor = colorscales[[input$colorscale]]$bgcolor) %>%
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
        main <- subplot(matrixheatmap(), rowmeta(), nrows = 1, shareY = T, margin = 0.01, widths = c(0.97, 0.03))
      } else {
        main <- subplot(colmeta(), plotly_empty(), matrixheatmap(), rowmeta(),
                        nrows = 2, shareX = T, shareY = T, margin = 0.01,
                        widths = c(0.97, 0.03), heights = c(0.03, 0.97))
      }
      main$x$source <- "main"
      main %>% plotly::config(displayModeBar = F)
    })


    #-- Drilldown handling -----------------------------------------------------------------------------------------------------#

    observeEvent(mdata$cdata, {
      updateSelectizeInput(session, "drilldown", choices = removeID(names(mdata$cdata)), selected = character(0))
    })

    observe({
      s <- event_data("plotly_click", source = "main")
      if(!is.null(s)) {
        var1 <- s[["x"]]
        var2 <- s[["y"]]
        updateSelectizeInput(session, "drilldown", "Drill down to data for", selected = c(var1, var2))
      }
    })

    output$scatter <- renderPlotly({
      req(input$drilldown != "")
      var1 <- input$drilldown[1]
      colorgroup <- names(dcolors) # default categorical group colors

      if(length(input$drilldown) == 2) { #-> do scatter plot 2-variable view ----------------------#
        var2 <- input$drilldown[2]
        datasub <- mdata$cdata[, c(..var1, ..var2, ..colorgroup)]
        datasub <- datasub[complete.cases(datasub)]
        if(factorx(var1)) datasub[[var1]] <- factor(datasub[[var1]])
        if(factorx(var2)) datasub[[var2]] <- factor(datasub[[var2]])
        drillplot2(datasub, var1, var2, dcolors, colorgroup, input$flipxy %% 2, factorx)

      } else { #-> do boxplot 1-variable view ----------------------------------------------------#
        datasub <- mdata$cdata[!is.na(get(var1)), c(..var1, ..colorgroup)]
        drillplot1(datasub, var1, colorgroup)
      }
    })
  })
}

# -- Helpers ---------------------------------------------------------------------------------------------------------#

# Generate meta marginal plots
metamargin <- function(x, y, z, name = "Group", text, colorscale = "Portland") {
  axis <- list(title = "", showgrid = F, showticklabels = F, ticks = "")
  plot_ly(type = "heatmap", x = x, y = y, z = z,
          name = name, text = text, hovertemplate = "<b>%{text}</b>",
          showscale = FALSE, colorscale = colorscale) %>%
    layout(xaxis = axis, yaxis = axis)
}

# Two-variable scatterplot
drillplot2 <- function(datasub, var1, var2, dcolors, colorgroup, flipxy, factorx) {
  p <- ggplot(datasub, aes_string(x = var1, y = var2)) +
    labs(title = paste("n =", nrow(datasub))) +
    theme_bw()
  # different plot when both variables are categorical
  if(all(factorx(c(var1, var2)))) {
    p <- p + geom_count()
  } else {
    p <- p + geom_point(aes_string(color = colorgroup), size = 2, alpha = 0.7)
  }
  if(colorgroup %in% names(dcolors)) p <- p + scale_colour_manual(values = dcolors[[colorgroup]])
  if(flipxy) p <- p + coord_flip()
  p <- suppressWarnings(ggplotly(p)) %>% plotly::config(displayModeBar = F)
  p
}

# "Single-variable" boxplots with a default factor group
drillplot1 <- function(datasub, var1, colorgroup) {
  p <- ggplot(datasub, aes_string(x = colorgroup, y = var1)) +
    geom_boxplot(outlier.color = NA) +
    scale_colour_manual(values = dcolors[[1]]) +
    labs(title = paste("n =", nrow(datasub))) +
    theme_bw()
  if(is.factor(datasub[[var1]])) {
    p <- p + geom_count(aes_string(color = colorgroup))
  } else {
    p <- p + geom_point(aes_string(color = colorgroup), size = 2, alpha = 0.5,
                        position = position_jitter(width = 0.05, height = 0.05))
  }
  p <- suppressWarnings(ggplotly(p)) %>% hide_legend() %>% layout(xaxis = list(tickangle = 45)) %>% plotly::config(displayModeBar = F)
  p$x$data[[1]]$marker$opacity <- 0 # manual specify since plotly doesn't translate this for boxplot
  p
}

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

#' @export
colorscale_heatmap_custom <- function(palette) {
  function(z) colorscale_heatmap_manual(z, palette = palette)
}


colorscale_heatmap_absolute <- function(z) {
  colorscale_heatmap_manual(z, palette = c(hcl.colors(10, "YlOrRd", rev = TRUE), hcl.colors(10, "YlOrRd")))
}

