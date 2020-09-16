#' Shiny module UI for presenting high dimensional data with other features in multi-column view
#'
#' Integratively visualize a high-dimensional expression dataset with phenotype or clinical features
#'
#' The UI is based on "visual spreadsheet" or "line up" view.
#' The original use case presumes the high-dimensional data to be expression data, e.g.
#' gene or protein expression matrices. See \code{\link{xVServer}} for details.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
xVUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "xvui",
           div(class = "xvui-ctrl",
               conditionalPanel(condition = "input.addlocal%2==1", ns = ns, class = "ui-inline",
                                selectizeInput(ns("localselected"), label = NULL, choices = NULL, multiple = T,
                                    options = list(placeholder = "filter in this dataset"))
                ),
               div(class = "ui-inline", actionLink(ns("addlocal"), "LOCAL FILTER", icon = icon("plus")),
                     title = "Initalize local filter for this dataset **that takes precedence over the global filter**"),
               div(class = "ui-inline", actionLink(ns("rowcluster"), "ROW CLUSTER", icon = icon("sitemap", class = "fa-rotate-270")),
                    title = "Cluster samples by **features currently in view**"),
               div(class = "ui-inline", "show most variable:"),
               div(class = "ui-inline", style = "margin-top: -10px;",
                   numericInput(ns("hivarprct"), label = NULL, value = 100, min = 1, max = 100, step = 5, width = 50)),
               div(class = "ui-inline", icon("percent"))
          ),
          fluidRow(
            column(9, plotlyOutput(ns("heatmap"))),
            column(3, plotlyOutput(ns("cplotly")))
          )
  )
}

#' Shiny module server for presenting high dimensional data with other features in multi-column view
#'
#' Integratively visualize a high-dimensional expression dataset with phenotype or clinical features
#'
#' The data objects here can be compared to \code{Biobase::ExpressionSet} objects,
#' where \code{hdata} corresponds to a (transposed) \code{exprs} matrix in the \code{assayData} slot and
#' \code{cdata} corresponds to the data in the \code{phenoData} slot.
#'
#' The capabilities implemented are clustering (of samples by features only) and data subsetting of.
#' The data subsetting of expression data works through a local selection, which takes precedence
#' when activated, or through whatever is passed into \code{selected}.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param hdata A numeric matrix with row and column names for heatmap representation.
#' @param cdata Optional, a \code{data.table} of characteristics data. See details.
#' @param key Name of key column for \code{cdata}, currently defaults to "ID".
#' @param selected A reactive vector used to subset the features (cols) of \code{hdata}.
#' @param height Height for data plots.
#' @export
xVServer <- function(id,
                     hdata, cdata = reactive(NULL), key = "ID",
                     selected = reactive(NULL), height = 500) {

  moduleServer(id, function(input, output, session) {

    localhdata <- reactiveVal(NULL) # can be modified by both obs_globalselect and obs_localselect
    withcluster <- FALSE

    updateSelectizeInput(session, "localselected", choices = colnames(hdata), server = T)

    #-- Local controls --------------------------------------------------------------------------------------------#

    # Local select is shown on user request and takes precedence over global select
    # To restore plot listening to global selection, the local select input must be removed from UI
    observeEvent(input$addlocal, {
      if(input$addlocal %% 2) {
        updateActionButton(session, "addlocal", "LOCAL FILTER", icon = icon("minus"))
        obs_globalselect$suspend()
        obs_localselect$resume()
      } else {
        updateSelectizeInput(session, "localselected", selected = "")
        updateActionButton(session, "addlocal", "LOCAL FILTER", icon = icon("plus"))
        obs_localselect$suspend()
        obs_globalselect$resume()
      }
    })

    obs_globalselect <- observe({
      if(!length(selected())) {
        localhdata(hdata)
      } else {
        localhdata( hdata[, colnames(hdata) %in% selected(), drop = F] )
      }
    })

    obs_localselect <- observe({
        if(!length(input$localselected)) {
          localhdata(hdata)
        } else {
          localhdata( hdata[, colnames(hdata) %in% input$localselected, drop = F] )
        }
    }, suspended = TRUE) # start in suspended state


    # Subsetting by highest-variance features w/ much checking bc numericInput doesn't handle non-sensible inputs well
    observeEvent(input$hivarprct, {
      if(!length(input$hivarprct) || !is.numeric(input$hivarprct)) {
        updateNumericInput(session, "hivarprct", value = 100)
      } else if(input$hivarprct < 1) {
        updateNumericInput(session, "hivarprct", value = 1)
      } else if(input$hivarprct > 100) {
        updateNumericInput(session, "hivarprct", value = 100)
      } else {
        selected <- subsetHiVar(hdata, percent = isolate(input$hivarprct))
        localhdata(hdata[, selected, drop = F])
      }
    }, ignoreInit = TRUE)

    #-- Plots ---------------------------------------------------------------------------------------------------------#

    #-- Expression matrix heatmap ---------------------------------------------------------------------------#

    # Initialize heatmap with empty left-hand subplot for dendrogram / most subsequent mods use plotlyProxy
    output$heatmap <- renderPlotly({
      axis_ <- list(showgrid = FALSE, zeroline = FALSE, ticks = "")
      d <- plot_ly(type = "scatter", mode = "markers", x = NULL, y = 1:nrow(hdata)) %>%
        layout(xaxis = c(axis_, showticklabels = FALSE), yaxis = axis_)
      subplot(d, expHeatmap(hdata, height), shareY = TRUE, widths = c(0, 1)) %>%
        plotly::config(displayModeBar = F)
    })


    # Add row cluster dendro subplot trace in left margin -- cluster dendogram is removed if cdata() changes
    observeEvent(input$rowcluster, {
      z <- localhdata()
      tryCatch({
        rowclusts <- rowCluster(z)
        z <- z[rowclusts$order, ]
        withcluster <<- TRUE
        localhdata(z) # obs_localhdata will then take care of updating heatmap trace 1L
        lines <- lineShapes(rowclusts)
        labels <- rowclusts$labels[rowclusts$order]
        plotlyProxy("heatmap", session) %>%
          plotlyProxyInvoke("update",
                            list(type = "scatter", mode = "markers+text",
                                 x = list(rep(0, length(labels))), y = list(1:length(labels)),
                                 text = list(labels), textposition="middle right", hoverinfo = "text",
                                 cliponaxis = FALSE),
                            list(xaxis.domain = c(0, 0.1), xaxis2.domain = c(0.13, 1),
                                 shapes = lines, xaxis.autorange = "reversed"),
                            0L)
      }, error = function(e) meh(error = e))

    }, ignoreInit = TRUE)


    # Update heatmap whenever matrix changes
    obs_localhdata <- observeEvent(localhdata(), {
      z <- localhdata()
      # quirk where single values need to be double arrays, see https://stackoverflow.com/a/57013847
      x <- if(ncol(z) == 1) list(colnames(z)) else colnames(z)
      ylabs <- rownames(z)
      yind <- 1:nrow(z)
      showticks <- !withcluster # if cluster dendogram also being displayed,  don't need labels
      plotlyProxy("heatmap", session) %>%
        plotlyProxyInvoke("update",
                          list(z = list(z), x = list(x), y = list(yind), height = height),
                          list(yaxis.showticklabels = showticks, yaxis.tickvals = yind, yaxis.ticktext = ylabs),
                          1L)
    }, ignoreInit = TRUE)


    #-- Clinical/phenotype variables --------------------------------------------------------------------#

    # Sample-order by pheno var in cdata "sort by" column
    observeEvent(cdata(), {
      # Use sample IDs in cdata() to order main matrix
      crows <- cdata()[as.character(get(key)) %in% rownames(localhdata()), unique(as.character(get(key)))]
      z <- localhdata()[crows, , drop = F]
      # Main matrix can only be ordered by one source at a time! remove cluster plot when cdata() applies
      withcluster <<- FALSE
      plotlyProxy("heatmap", session) %>%
        plotlyProxyInvoke("update",
                          list(y = list(1:nrow(z)), text = NULL),
                          list(xaxis.domain = c(0, 0), xaxis2.domain = c(0, 1), shapes = NULL),
                          0L)
      localhdata(z)

    }, ignoreInit = T) # on init, cplot ordered by matrixplot rather than matrixplot ordered by cdata()


    # Plot product representing cdata(), which contains one to several plots of each var in subplot
    cplot <- reactive({
      if(is.null(cdata())) return(NULL)
      dtorder <- as.data.table(setNames(list(rownames(localhdata())), key))
      plotdata <- merge(dtorder, cdata(), by = key, all.x = T, sort = FALSE)
      plotdata <- plotdata[!duplicated(get(key))] # in case cdata() isn't well-cleaned
      y <- plotdata[[key]] %>% paste()
      notID <- names(cdata()) != key
      vcat <- sapply(cdata(), function(v) class(v) == "character" || class(v) == "factor")

      # Generate a list of plotly plots for categorical variables
      cplotcat <- lapply(names(plotdata)[vcat & notID],
                         function(v) vcatplotly(v, cdata()[[v]], plotdata[[v]], y, height))
      # Generate a list of plotly plots for numeric variables
      cplotnum <- lapply(names(plotdata)[!vcat & notID], function(v) vnumplotly(v, plotdata[[v]], y, height))

      if(length(cplotcat) && length(cplotnum)) {
        # To look best numeric plots need more width allocated, use widths below for now
        subplot(subplot(cplotcat, shareY = T, titleX = T), subplot(cplotnum, shareY = T, titleX = T),
                titleX = T, shareY = T, widths = c(0.3, 0.7)) %>%
          plotly::config(displayModeBar = F)
      } else {
         subplot(c(cplotcat, cplotnum), shareY = T, titleX = T) %>%
          layout(margin = list(t = 0.071 * height, b = 0.077 * height)) %>% # https://github.com/plotly/plotly.js/issues/4583
          plotly::config(displayModeBar = F)
      }
    })

    output$cplotly <- renderPlotly({
      cplot()
    })

  })
}

#-- Helper functions -----------------------------------------------------------------------------------#

# Expression heatmap of N samples x p features
expHeatmap <- function(z, height) {
  xlabs <- colnames(z)
  ylabs <- rownames(z)
  yind <- 1:nrow(z) # keep y as integer indices instead of using names for dendogram compatibility
  # infer color scale for type of data; min = 0 -> RNA-seq counts; everything else should be log-Fc
  if(min(z) == 0) colorscale <- "Greys" else colorscale <- "RdBu"
  # only show labels when readable
  showticklabs <- if(ncol(z) <= 50) TRUE else FALSE

  plot_ly(z = z, x = xlabs, y = yind, name = "Expression\nMatrix",
          type = "heatmap", colors = colorscale, height = height,
          # text = matrix(rep(ylabs, each = ncol(z)), ncol = ncol(z), byrow = T),
          # hovertemplate = "feature: <b>%{x}</b><br>expression: <b>%{z}</b>",
          colorbar = list(title = "relative\nexpression", thickness = 10, x = -0.09)) %>%
    layout(xaxis = list(type = "category", showgrid = FALSE, ticks = "", showticklabels = showticklabs),
           yaxis = list(ticks = "", tickvals = yind, ticktext = ylabs))
}


# For clustering samples
rowCluster <- function(data) {
  withProgress(value = 0.2, message = "creating distance matrix...",
               expr = {
                 clusts <- dist(data)
                 setProgress(value = 0.7, message = "clustering...")
                 clusts <- fastcluster::hclust(clusts)
                 return(clusts)
               })
}

# Get line segments list from a cluster object, which can be passed into plotly shapes
lineShapes <- function(cluster, horiz = TRUE) {
  dendata <- dendextend::as.ggdend(as.dendrogram(cluster))
  segments <- dendata$segments
  names(segments) <- if(horiz) c("y0", "x0", "y1", "x1") else c("x0", "y0", "x1", "y1")
  linetype <- list(line = list(color = "gray", width = 2), type = "line", xref = "x", yref = "y")
  lines <- apply(segments, 1, function(x) c(as.list(x), linetype))
  return(lines)
}

# Simple plot of a vector of categorical variables
# vt is the variable name used in plot title; vu is the un-subsetted vec; vx is the subsetted vec
vcatplotly <- function(vt, vu, vx, y, height = NULL) {
  zdomain <- vu %>% factor() %>% levels()  # keep colors consistent across xVUIs
  z <- vx %>% factor(levels = zdomain) %>% as.integer() %>% as.matrix()
  colorscale <- if(length(zdomain) < 4) "Viridis" else "Portland"
  text <- matrix(paste(y, "|", "value =", vx))
  plot_ly(x = vt, y = y, z = z, name = vt, type = "heatmap", height = height,
          text = text, hoverinfo = "text", showscale = FALSE,
          colorscale = colorscale, zmin = 1, zmax = length(zdomain)) %>%
    layout(xaxis = list(title = vt, zeroline = FALSE, showline = FALSE,
                        showticklabels = FALSE, showgrid = FALSE, type = "category"),
           yaxis = list(type = "category", categoryorder = "array", categoryarray = y))
}

# Both plotly and ggplot will remove NA data automatically, which isn't desired default
# Instead, use a dummy "0" value and add annotation layer to indicate where data is missing
vnumplotly <- function(vt, vx, y, height = NULL) {
  x <- vx
  hoverx <- paste(y, "|", sapply(x, function(i) if(is.na(i)) "NA" else as.character(i)))
  NAtext <- ifelse(is.na(x), "  NA", "")
  x[is.na(x)] <- 0
  plot_ly(x = x, y = y, customdata = hoverx, name = vt, type = "bar",
          orientation = "h", showlegend = F, height = height,
          text = hoverx, hoverinfo = "text") %>%
    add_text(text = NAtext, textposition = "right", textfont = list(color = toRGB("red"))) %>%
    layout(xaxis = list(title = vt, showgrid = FALSE),
           yaxis = list(type = "category", categoryorder = "array", categoryarray = y))
}


# Subset a matrix by the most variable n features (columns)
# Need to either give n number of features or percent of data to calculate n
subsetHiVar <- function(data, n = NULL, percent) {
  n <- round(ncol(data) * (percent/100))
  n <- ifelse(n < 1, 1, n)
  vars <- apply(data, 2, var)
  selected <- order(vars, decreasing = T)[1:n]
  return(selected)
}

