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
  fluidRow(style="margin-top:30px; margin-bottom:20px; margin-right:100px",
                     column(8, plotlyOutput(ns("heatmap")),
                     column(1, ""),
                     column(1, ""),
                     column(1 ),
                     column(1 )
          )
  )
}

#' Shiny module server for presenting high dimensional data with other features in multi-column view
#'
#' This module attempts to integratively visualize one or more 
#' high-dimensional gene/protein/methylation datasets with other phenotype or clinical features.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param hdata A numeric matrix of of the high dimensional data.
#' @param cdata Data.frame or data.table that can be joined to high-dimensional data. See details.
#' @param selected A vector used to subset the columns of hdata.
#' @param slabel Optional, a vector that can map column names of hdata to plot labels.
#' @export
multiV <- function(input, output, session,
                   hdata, cdata, selected, slabel = NULL) {
  
  plotdata <- reactive({
    if(!length(selected())) hdata else hdata[, colnames(hdata) %in% selected(), drop = F]
  })
  
  output$heatmap <- renderPlotly({
    xlabs <- if(!is.null(slabel)) slabel[colnames(plotdata())] else colnames(plotdata())
    ylabs <- rownames(plotdata())
    plot_ly(z = plotdata(), x = xlabs, y = ylabs, type = "heatmap", colors = "RdBu") %>% 
      layout(xaxis = list(type = "category"), yaxis = list(type = "category"))
  })
  
}