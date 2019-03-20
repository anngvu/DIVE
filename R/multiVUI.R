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
                   hdata, cdata = NULL, selected, slabel = NULL) {

  hplot <- reactive({
    if(!length(selected())) hdata else hdata[, colnames(hdata) %in% selected(), drop = F]
  })

  cplot <- reactive({
    if(!is.null(cdata)) cdata()[as.character(ID) %in% rownames(hdata), ] else return(NULL)
  })

  output$heatmap <- renderPlotly({
    y <- as.character(cplot()[["ID"]])
    notID <- names(cplot()) != "ID"
    vcat <- sapply(cplot(), function(v) class(v) == "character" | class(v) == "factor")

    cplotlist1 <- lapply(names(cplot())[vcat & notID],
                         function(v) {
                             plot_ly(x = 1, y = y, name = v, type = "bar", orientation = "h") %>%
                             layout(xaxis = list(title = v, zeroline = FALSE, showline = FALSE,
                                                 showticklabels = FALSE, showgrid = FALSE),
                                    yaxis = list(type = "category"))
                         }
    )
    cplotlist2 <- lapply(names(cplot())[!vcat & notID],
                         function(v) {
                            x <- cplot()[[v]]
                            x[is.na(x)] <- 0
                            plot_ly(x = x, y = y, name = v, type = "bar", orientation = "h") %>%
                            layout(xaxis = list(title = v), yaxis = list(type = "category"))
                         }
    )

    cplots <- subplot(subplot(cplotlist1, shareY = T, titleX = T), subplot(cplotlist2, shareY = T, titleX = T),
                      titleX = T, shareY = T, widths = c(0.3, 0.7))

    xlabs <- if(!is.null(slabel)) slabel[colnames(hplot())] else colnames(hplot())
    ylabs <- rownames(hplot())
    h <- plot_ly(z = hplot(), x = xlabs, y = ylabs, type = "heatmap", colors = "RdBu", height = 28 * nrow(hdata)) %>%
      layout(xaxis = list(type = "category"), yaxis = list(type = "category"),
             plot_bgcolor = "#F5F5F5")
    subplot(cplots, h, titleX = T, shareY = T, widths = c(0.3, 0.7)) %>%
      config(displayModeBar = F)
  })

}
