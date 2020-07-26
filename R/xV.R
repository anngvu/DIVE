#' Shiny module UI for presenting high dimensional data with other features in multi-column view
#'
#' The UI is based on the idea of a "visual spreadsheet" or "line up" view. See associated
#' server function \code{\link{multiV}} for details of the implementation.
#'
#' The original use case presumes the high dimensional data to be expression data, e.g.
#' gene or protein expression matrices.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
xVUI <- function(id) {
  ns <- NS(id)
  tags$div(style = "margin-bottom:30px;",
           div(style = "margin-bottom: 10px;",
               div(class = "forceInline", actionButton(ns("show"), label = NULL, icon = icon("cog"))),
               conditionalPanel(condition = "input.show%2==1", ns = ns, class = "forceInline",
                div(class = "forceInline", id = ns("local")),
                div(class = "forceInline", actionLink(ns("addlocal"), "LOCAL FILTER", icon = icon("plus"))),
                div(class = "forceInline", actionLink(ns("rowcluster"), "ROW CLUSTER", icon = icon("sitemap"))),
                # TO DO: further testing of col clustering
                # div(class = "forceInline", actionLink(ns("colcluster"), "COL CLUSTER", icon = icon("sitemap"))),
                div(class = "forceInline", "show most variable"),
                div(style = "display: inline-block; margin-top: -5px;",
                    numericInput(ns("hivarprct"), label = NULL, value = 100, min = 1, max = 100, step = 5, width = 50)),
                div(style = "display: inline-block;", icon("percent"))
              )
          ),
          div(shinycssloaders::withSpinner(plotlyOutput(ns("heatmap"), height = "100%"), color = "gray"))
  )
}

#' Shiny module server for presenting high dimensional data with other features in multi-column view
#'
#' This module attempts to integratively visualize one or more
#' high-dimensional gene/protein/methylation datasets with other phenotype or clinical features.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param hdata A numeric matrix representing high dimensional data.
#' @param cdata Optional, a data.frame or data.table that can be joined to high-dimensional data. See details.
#' @param key Name of key column, should be and currently defaults to "ID".
#' @param selected A vector used to subset the columns of hdata.
#' @param slabel Optional, a vector that can map column names of hdata to plot labels.
#' @param width Custom width, defaults to 1600, only used when dendro subplots are involved.
#' @export
xVServer <- function(id,
                     hdata, cdata = reactive({ NULL }), key = "ID",
                     selected = reactive({ NULL }), slabel = NULL,
                     width = reactive(1600)) {

  moduleServer(id, function(input, output, session) {

    localhdata <- reactiveVal(NULL)
    clusterplot <- reactiveVal(NULL)
    hivarprct <- reactiveVal(100)

    #-- Local controls --------------------------------------------------------------------------------------------#

    # Local select is dynamically rendered on user request and takes precedence over global select
    # To restore plot listening to global selection, local select must be removed
    observeEvent(input$addlocal, {
      if(input$addlocal %% 2) {
        insertUI(selector = paste0("#", session$ns("local")), immediate = T,
                 ui = tags$div(id = session$ns("localselected"),
                               selectizeInput(session$ns("localselected"), label = NULL,
                                              choices = NULL, multiple = T,
                                              options = list(placeholder = "filter in this dataset"))))
        updateSelectizeInput(session, "localselected", choices = colnames(hdata), server = T)
        updateActionButton(session, "addlocal", "LOCAL FILTER", icon = icon("minus"))
        obs_globalselect$suspend()
        obs_localselect$resume()
      } else {
        removeUI(selector = paste0("#", session$ns("localselected")))
        updateActionButton(session, "addlocal", "LOCAL FILTER", icon = icon("plus"))
        obs_localselect$suspend()
        obs_globalselect$resume()
        selected(selected())
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


    # Subsetting by highest-variance features and constraining user input
    # Shiny's numeric input doesn't handle non-sensible input well by default
    observe({
      if(!length(input$hivarprct) || !is.numeric(input$hivarprct)) {
        updateNumericInput(session, "hivarprct", value = 100)
      } else if(input$hivarprct < 1) {
        updateNumericInput(session, "hivarprct", value = 1)
      } else if(input$hivarprct > 100) {
        updateNumericInput(session, "hivarprct", value = 100)
      } else {
        hivarprct(input$hivarprct)
      }
    })

    #-- Plots ---------------------------------------------------------------------------------------------------------#


    #-- Expression matrix ---------------------------------------------------------------------------#


    hplot <- reactive({
      if(is.null(localhdata())) return(NULL)
      plotdata <- localhdata()
      selected <- subHiVar(plotdata, percent = hivarprct())  # plot highest variance
      plotdata <- plotdata[, selected, drop = F] # plotdata[, selected, drop = F] gives subscript out of bounds
      xlabs <- if(!is.null(slabel)) slabel[colnames(plotdata)] else colnames(plotdata)
      ylabs <- rownames(plotdata)
      showticklabs <- if(ncol(plotdata) <= 50) TRUE else FALSE # only show labels when readable

      # infer color scale for type of data; min = 0 -> RNA-seq counts; everything else should be log-FC
      if(min(plotdata) == 0) colorscale <- "Greys" else colorscale <- "RdBu"
      plot_ly(z = plotdata, x = xlabs, y = ylabs, name = "Expression\nMatrix",
              type = "heatmap", height = 25 * nrow(hdata), colors = colorscale,
              hovertemplate = "transcript/protein: <b>%{x}</b><br>sampleID: <b>%{y}</b><br>expression: <b>%{z}</b>",
              colorbar = list(title = "relative\nexpression", thickness = 10, x = -0.09)) %>%
        layout(xaxis = list(type = "category", showgrid = FALSE, ticks = "", showticklabels = showticklabs),
               yaxis = list(type = "category", ticks = ""),
               plot_bgcolor = "#F5F5F5")
    })

    #-- Cluster/dendro plot ---------------------------------------------------------------------------#
    observeEvent(input$rowcluster, {
      withProgress(value = 0.2, message = "creating distance matrix...",
                   expr = {
                     tryCatch({
                       sample_clust <- dist(hdata) # cluster by samples
                       setProgress(value = 0.7, message = "clustering...")
                       sample_clust <- fastcluster::hclust(sample_clust)
                       setProgress(value = 0.9, message = "reordering rows...")
                       localhdata(hdata[sample_clust$labels[sample_clust$order], ])
                       # add new dendroplot
                       dendro <- plotly::plot_dendro(as.dendrogram(sample_clust), width = width()-40)
                       dendro <- dendro %>% layout(xaxis = list(autorange = "reversed", showgrid = FALSE),
                                                   yaxis = list(showgrid = FALSE),
                                                   paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
                                                   margin = list(b = 0, t = 0, r = 25, l = 10))
                       # hide labels and legend for cleaner display but add hover:
                       dendro$x$attrs <- lapply(dendro$x$attrs,  function(x) {
                         x$showlegend <- F
                         if(!is.null(x$name) && x$name == "labels") {
                           x$name <- "sample"
                           x$color <- sub("black", "transparent", x$color)
                         }
                         x$hoverinfo <- x$text
                         x
                       })
                       clusterplot(dendro)
                     }, error = function(e) meh(error = e))
                   })
    })

    observeEvent(input$colcluster, {
      withProgress(value = 0.2, message = "creating distance matrix...",
                   expr = {
                     tryCatch({
                       gene_clust <- dist(t(hdata))
                       setProgress(value = 0.7, message = "clustering...")
                       gene_clust <- fastcluster::hclust(gene_clust)
                       setProgress(value = 0.9, message = "reordering columns...")
                       hdata <- hdata[, gene_clust$order]
                       localhdata(hdata)
                     }, error = function(e) meh())
                   })
    })


    #-- Clinical/phenotype variables --------------------------------------------------------------------#

    # Plot subsetting and sample-ordering by phenotype variables
    observeEvent(cdata(), {
      # manually order rows by order given in cdata(); cdata() is itself ordered by a "sort by" column
      ckey <- cdata()[as.character(get(key)) %in% rownames(localhdata()), as.character(get(key))]
      plotdata <- localhdata()[ckey, , drop = F]
      localhdata(plotdata)
      clusterplot(NULL) # can only use one ordering...
    }, ignoreInit = T) # let cplot follow order of hplot rather than hplot follow cdata() when first initialized


    cplot <- reactive({
      # cdata() should be a table already subsetted by selected variables (columns)
      if(is.null(cdata())) return(NULL)
      plotdata <- merge(as.data.table(setNames(list(rownames(localhdata())), key)), cdata(),
                        by = key, all.x = T, sort = FALSE)
      plotdata <- plotdata[!duplicated(get(key))]
      # print(plotdata)
      y <- plotdata[[key]] %>% paste()
      notID <- names(cdata()) != key
      vcat <- sapply(cdata(), function(v) class(v) == "character" || class(v) == "factor")

      # Generate a list of plotly plots for categorical variables
      cplotcat <- lapply(names(plotdata)[vcat & notID],
                         function(v) {
                           zdomain <- cdata()[[v]] %>% factor() %>% levels()  # keep colors consistent across xVUIs
                           colorscale <- if(length(zdomain) < 4) "Viridis" else "Portland"
                           z <- plotdata[[v]] %>% factor(levels = zdomain) %>% as.integer() %>% as.matrix()
                           text <- matrix(paste(y, "|", "value =", plotdata[[v]]))
                           plot_ly(x = v, y = y, z = z, name = v, type = "heatmap",
                                   showscale = FALSE, text = text, hoverinfo = "text",
                                   colorscale = colorscale, zmin = 1, zmax = length(zdomain)) %>%
                             layout(xaxis = list(title = v, zeroline = FALSE, showline = FALSE,
                                    showticklabels = FALSE, showgrid = FALSE, type = "category"),
                                    yaxis = list(type = "category", categoryorder = "array", categoryarray = y))
                        })

      # Generate a list of plotly plots for numeric variables
      # Both plotly and ggplot will remove NA data automatically, which isn't great for our purpose.
      # Instead, we use a dummy "0" value and add annotation layer to indicate where data is missing.
      cplotnum <- lapply(names(plotdata)[!vcat & notID],
                         function(v) {
                           x <- plotdata[[v]]
                           hoverx <- paste(y, "|", sapply(x, function(i) if(is.na(i)) "NA" else as.character(i)))
                           NAtext <- ifelse(is.na(x), "  NA", "")
                           x[is.na(x)] <- 0
                           plot_ly(x = x, y = y, customdata = hoverx, name = v,
                                   type = "bar", orientation = "h", showlegend = F,
                                   text = hoverx, hoverinfo = "text") %>%
                                   add_text(text = NAtext, textposition = "right", textfont = list(color = toRGB("red"))) %>%
                                   layout(xaxis = list(title = v, showgrid = FALSE),
                                        yaxis = list(type = "category", categoryorder = "array", categoryarray = y))
                      })

      if(length(cplotcat) && length(cplotnum)) {
        # Technically, numeric plots need more width allocated to look most effective, but use method below for now
        # n <- length(cplotcat) + length(cplotnum)
        # w1 <- round(length(cplotcat)/(length(cplotcat) + length(cplotnum)))
        # widths <- c(w1, n-w1)
        subplot(subplot(cplotcat, shareY = T, titleX = T),
                 subplot(cplotnum, shareY = T, titleX = T),
                 titleX = T, shareY = T, widths = c(0.3, 0.7))
      } else {
         subplot(c(cplotcat, cplotnum), shareY = T, titleX = T)
      }
    })

    #-- Combined plots --------------------------------------------------------------------#

    output$heatmap <- renderPlotly({
      if(is.null(cplot()) && is.null(clusterplot())) {
          hplot() %>% plotly::config(displayModeBar = F)
      } else if(is.null(cplot())) {
          subplot(clusterplot(), hplot(), shareX = T, widths = c(0.1, 0.9)) %>%
          plotly::config(displayModeBar = F)
      } else if(is.null(clusterplot())) {
          subplot(hplot(), cplot(), titleX = T, shareX = F, shareY = T, widths = c(0.7, 0.3)) %>%
            plotly::config(displayModeBar = F)
      } else if(is.null(hplot())) { # all plots are removed if hplot() is null so really moot option
        # cplot() %>% plotly::config(displayModeBar = F)
      } else {
        # the most complicated configuration:
        subplot(subplot(clusterplot(), hplot(), shareX = T, widths = c(0.1, 0.9)),
                cplot(), titleX = T, shareY = F, widths = c(0.7, 0.3)) %>%
          plotly::config(displayModeBar = F, autosizable = TRUE)
      }
    })
  })

}

#-- Helper functions -----------------------------------------------------------------------------------#

# Subset a matrix by the most variable n features (columns)
# Need to either give n number of features or percent of data to calculate n
subHiVar <- function(data, n = NULL, percent) {
  n <- round(ncol(data) * (percent/100))
  n <- ifelse(n < 1, 1, n)
  vars <- apply(data, 2, var)
  selected <- order(vars, decreasing = T)[1:n]
  return(selected)
}

