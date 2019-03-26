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
  tags$div(style="margin-top:10px; margin-bottom:10px; margin-right:50px",
           shinycssloaders::withSpinner(plotlyOutput(ns("heatmap")), color = "gray")
  )
}

#' Shiny module server for presenting high dimensional data with other features in multi-column view
#'
#' This module attempts to integratively visualize one or more
#' high-dimensional gene/protein/methylation datasets with other phenotype or clinical features.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param hdata A numeric matrix of of the high dimensional data.
#' @param cdata Optional, a data.frame or data.table that can be joined to high-dimensional data. See details.
#' @param selected A vector used to subset the columns of hdata.
#' @param slabel Optional, a vector that can map column names of hdata to plot labels.
#' @export
multiV <- function(input, output, session,
                   hdata, cdata = reactive({ NULL }), key = "ID", selected = reactive({ NULL }), slabel = NULL) {

  hplotdata <- reactive({
    plotdata <- if(!length(selected())) hdata else hdata[, colnames(hdata) %in% selected(), drop = F]
    if(!is.null(cdata())) {
      # manually order rows by order given in cdata()
      ckey <- cdata()[as.character(get(key)) %in% rownames(hdata), as.character(get(key))]
      plotdata <- plotdata[ckey, , drop = F]
    }
    plotdata
  })

  hplot <- reactive({
    # if(!length(hplotdata())) return(NULL)
    xlabs <- if(!is.null(slabel)) slabel[colnames(hplotdata())] else colnames(hplotdata())
    ylabs <- rownames(hplotdata())
    showticklabs <- if(length(hplotdata()) <= 50) TRUE else FALSE # only show labels when readable
    plot_ly(z = hplotdata(), x = xlabs, y = ylabs, # name = "relative expression",
            type = "heatmap", colors = "RdBu", height = 25 * nrow(hdata),
            # text = paste(~y, "\nsampleID": ~x, "\nrelative expression:", ~z),
            # hoverinfo = "text",
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
                             # Note: there's a plotly issue that will make it spew a lot of warnings
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
