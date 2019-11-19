#' Shiny UI Output for interactive hybrid parallel coordinates graph
#'
#' The module embeds an interactive "hybrid parallel coordinates graph" built
#' with \code{\link{hybridParCoordsGraph}}. Categorical nodes are linked to a pie chart
#' that gives more information on user hover.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return \preformatted{div} component to contain graphs.
#' @export
HPCGraphOutput  <- function(id) {
  ns <- NS(id)
  tags$div(id = "cohortGraph", style = "height:500px;",
    fluidRow(
    column(2, style="margin-right:-40px; padding-top:30px;", plotlyOutput(ns("pie"))),
    column(9, shinycssloaders::withSpinner(color = "gray", plotlyOutput(ns("graph")))),
    column(1)
  ))
}

#' Shiny module server for interactive hybrid parallel coordinates graph
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param hpcg Data object produced by \code{\link{newHPCG}}.
#' @export
HPCGraph <- function(input, output, session,
                     hpcg, colors, plotbgcolor = NULL) {

  output$graph <- renderPlotly({
    HPCG <- plotlyHPCG(hpcg, colors, plotbgcolor)
    HPCG %>% plotly::config(displayModeBar = F)
  })

  output$pie <- renderPlotly({
    hover <- event_data("plotly_hover")
    if(is.null(hover$key)) return()
    if(hover$key == "NA") {
      pie <- hpcg$plot_data[variable == hover$x & value_scaled == hover$y, table(color)]
    } else {
      pie <- hpcg$plot_data[variable == hover$key & value_scaled == hover$y, table(color)]
    }
    pie <- melt(pie, measure = names(pie))
    pie <- pie[order(pie$value), ]
    piecolors <- as.character(pie$color[order(pie$value)])
    piecolors <- colors[piecolors] # coloring specified manually
    piecolors <- apply(col2rgb(piecolors), 2, function(x) paste0("rgb(", x[1], ",", x[2],",", x[3], ")"))
    p <- plot_ly(pie, labels = ~color, values = ~value, type = 'pie', sort = F,
                 textinfo = "label", hole = 0.3, showlegend = F,
                 marker = list(colors = piecolors),
                 width = 300, height = 300,
                 insidetextfont = list(color = "#FFFFFF")) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor= "transparent", plot_bgcolor = "transparent",
             autosize = F, margin = list(t = 150, b = 20, r = 100, l = 70),
             font = list(size = 10))
    p
  })
}
