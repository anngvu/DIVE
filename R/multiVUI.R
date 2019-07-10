#' Shiny module UI for presenting high dimensional data with other features in multi-column view
#'
#' UI based on the idea of a "visual spreadsheet" or "line up" view. The associated
#' server function \code{\link{multiV}} provides more details of the implementation.
#'
#' The original use case presumes the high dimensional data to be genomics data, e.g.
#' gene or protein expression that can be represented with heatmap matrices.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
multiVUI <- function(id) {
  ns <- NS(id)
  tags$div(style = "margin-bottom:30px;",
    div(style = "margin-bottom: 10px;",
        div(class = "forceInline", actionButton(ns("show"), label = NULL, icon = icon("cog"))),
        conditionalPanel(condition = "input.show%2==1", ns = ns, class = "forceInline",
          div(class = "forceInline", id = ns("custom")),
          div(class = "forceInline", actionLink(ns("addcustom"), "LOCAL FILTER", icon = icon("plus"))),
          div(class = "forceInline", actionLink(ns("cluster"), "CLUSTER", icon = icon("sitemap"))),
          div(class = "forceInline", "Show most variable"),
          div(style = "display: inline-block; margin-top: -5px;",
              numericInput(ns("hivarprct"), label = NULL, value = 10, min = 1, max = 100, step = 5, width = 50)),
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
#' @param selected A vector used to subset the columns of hdata.
#' @param slabel Optional, a vector that can map column names of hdata to plot labels.
#' @export
multiV <- function(input, output, session,
                   hdata, cdata = reactive({ NULL }), key = "ID", selected = reactive({ NULL }), slabel = NULL) {

  localselect <- reactiveVal(NULL)
  localhdata <- reactiveVal(hdata)
  hivarprct <- reactiveVal(10)

  #-- Clustering -----------------------------------------------------------------------------------------------------#
  observeEvent(input$cluster, {
    withProgress(value = 0.2, message = "creating distance matrix...",
      expr = {
              gene_clust <- dist(t(hdata))
              setProgress(value = 0.7, message = "clustering...")
              gene_clust <- hclust(gene_clust)
              setProgress(value = 0.9, message = "reordering columns...")
              hdata <- hdata[, gene_clust$order]
              localhdata(hdata)
              })
  })

  #-- Custom select --------------------------------------------------------------------------------------------------#
  observeEvent(input$addcustom, {
    if(input$addcustom %% 2) {
      insertUI(selector = paste0("#", session$ns("custom")), immediate = T,
               ui = tags$div(id = session$ns("customselect"),
                             selectizeInput(session$ns("customselect"), label = NULL, choices = NULL,
                                            multiple = T, options = list(placeholder = "filter in this dataset"))))
      updateSelectizeInput(session, "customselect", choices = colnames(hdata), server = T)
      updateActionButton(session, "addcustom", "LOCAL FILTER", icon = icon("minus"))
    } else {
      removeUI(selector = paste0("#", session$ns("customselect")))
      localselect(NULL)
      selected(selected())
      updateActionButton(session, "addcustom", "LOCAL FILTER", icon = icon("plus"))
    }
  })

  observeEvent(input$customselect, {
    localselect(input$customselect)
  })

  #-- Plots ----------------------------------------------------------------------------------------------------------#

  # Global vs. local selection control
  # local select takes precedence over global select; when local select option is displayed, global subsetting is ignored
  # to restore plot listening to global selection, local select must be removed
  observe({
    if(is.null(localselect())) { if(!length(selected())) localhdata(hdata) else localhdata(hdata[, colnames(hdata) %in% selected(), drop = F]) }
  })

  observe({
    if(!length(localselect())) localhdata(hdata) else localhdata(hdata[, colnames(hdata) %in% localselect(), drop = F])
  })

  # Subsetting by highest variance features and constraining user input
  observe({
    if(!length(input$hivarprct) | !is.numeric(input$hivarprct)) {
      updateNumericInput(session, "hivarprct", value = 10)
    } else if(input$hivarprct < 0.1) {
      updateNumericInput(session, "hivarprct", value = 1)
    } else if(input$hivarprct > 100) {
      updateNumericInput(session, "hivarprct", value = 100)
    } else {
      hivarprct(input$hivarprct)
    }
  })

  # Plot subsetting by phenotype variables
  hplotdata <- reactive({
    plotdata <- localhdata()
    if(!is.null(cdata())) { # manually order rows by order given in cdata()
      ckey <- cdata()[as.character(get(key)) %in% rownames(hdata), as.character(get(key))]
      plotdata <- plotdata[ckey, , drop = F]
    }
    # subset to highest variance
    selected <- subHiVar(plotdata, percent = hivarprct())
    plotdata[, colnames(plotdata) %in% selected, drop = F] # for some reason plotdata[, selected, drop = F] gives subscript out of bounds
  })

  hplot <- reactive({
    xlabs <- if(!is.null(slabel)) slabel[colnames(hplotdata())] else colnames(hplotdata())
    ylabs <- rownames(hplotdata())
    showticklabs <- if(ncol(hplotdata()) <= 50) TRUE else FALSE # only show labels when readable
    # infer color scale for type of data
    if(min(hplotdata()) == 0) colorscale <- "Greys" else colorscale <- "RdBu"
    plot_ly(z = hplotdata(), x = xlabs, y = ylabs, name = "Expression\nMatrix",
            type = "heatmap", height = 25 * nrow(hdata), colors = colorscale,
            hovertemplate = "transcript/protein: <b>%{x}</b><br>sampleID: <b>%{y}</b><br>expression value: <b>%{z}</b>",
            colorbar = list(title = "relative\nexpression", thickness = 10, x = -0.09)) %>%
      layout(xaxis = list(type = "category", showgrid = FALSE, ticks = "", showticklabels = showticklabs),
            yaxis = list(type = "category", ticks = ""),
            plot_bgcolor = "#F5F5F5")
  })

  cplotdata <- reactive({
    if(is.null(cdata())) return(NULL)
    if(!is.null(hplotdata())) {
      plotdata <- cdata()[as.character(get(key)) %in% rownames(hplotdata()), ]
    } else {
      plotdata <- cdata()
    }
    plotdata
  })

  cplot <- reactive({
    if(is.null(cplotdata())) return(NULL)
    plotdata <- cplotdata()
    y <- as.character(plotdata[[key]])
    notID <- names(plotdata) != key
    vcat <- sapply(plotdata, function(v) class(v) == "character" | class(v) == "factor")
    cplotcat <- lapply(names(plotdata)[vcat & notID],
                           function(v) {
                             # Note: there's a plotly issue that will spew many warnings
                             plot_ly(x = 1, y = y, name = v, type = "bar", orientation = "h", showlegend = F,
                                     color = factor(plotdata[[v]]), text = plotdata[[v]],
                                     hoverinfo = "text") %>%
                               layout(xaxis = list(title = v, zeroline = FALSE, showline = FALSE,
                                                   showticklabels = FALSE, showgrid = FALSE),
                                      yaxis = list(type = "category", categoryorder = "array", categoryarray = y))
                         })
    cplotnum <- lapply(names(plotdata)[!vcat & notID],
                           function(v) {
                             x <- plotdata[[v]]
                             hoverx <- sapply(x, function(p) if(is.na(p)) "NA" else as.character(p))
                             NAtext <- ifelse(is.na(x), "  NA", "")
                             x[is.na(x)] <- 0
                             plot_ly(x = x, y = y, customdata = hoverx, name = v, type = "bar", orientation = "h", showlegend = F,
                                     text = hoverx, hoverinfo = "text") %>%
                               add_text(text = NAtext, textposition = "right", textfont = list(color = toRGB("red"))) %>%
                               layout(xaxis = list(title = v, showgrid = FALSE),
                                      yaxis = list(type = "category", categoryorder = "array", categoryarray = y))
                           })
    if(length(cplotcat) && length(cplotnum)) {
       subplot(subplot(cplotcat, shareY = T, titleX = T),
               subplot(cplotnum, shareY = T, titleX = T),
               titleX = T, shareY = T, widths = c(0.3, 0.7))
    } else {
       subplot(c(cplotcat, cplotnum), shareY = T, titleX = T)
    }
  })

  output$heatmap <- renderPlotly({
    if(is.null(cplot())) {
      hplot() %>% config(displayModeBar = F)
    } else if(is.null(hplot())) {
      cplot() %>% config(displayModeBar = F)
    } else {
      subplot(hplot(), cplot(), titleX = T, shareY = T, widths = c(0.7, 0.3)) %>%
        config(displayModeBar = F)
    }
  })

}

# Subset a matrix by the most variable n features (columns)
subHiVar <- function(data, n, percent) {
  n <- round(ncol(data) * (percent/100))
  n <- ifelse(n < 1, 1, n)
  vars <- apply(data, 2, var)
  selected <- names(sort(vars, decreasing = T))[1:n]
  return(selected)
}

