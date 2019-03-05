#' Shiny UI Output for interactive cohort graph
#'
#' The module embeds an interactive "hybrid parallel coordinates" graph pre-built,
#' linked with a pie chart that gives more information on a node hover.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return A \preformatted{div} for the plots.
#' @export
cohortGraphOutput  <- function(id) {
  ns <- NS(id)
  tags$div(id = "cohortGraph", style = "height:500px;",
    fluidRow(
    column(2, style="margin-right:-40px; padding-top:25px;", plotlyOutput(ns("cohPie"))),
    column(9, shinycssloaders::withSpinner(color = "gray", plotlyOutput(ns("cohGraph")))),
    column(1)
  ))
}

#' Shiny module server for interactive cohort graph
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param cohgraph Pre-built plotly graph widget.
#' @param cohdata Accompanying data.table of cohort data graph was built off.
#' @return A hybrid parallel axes graph for exploring a dataset (typically cohort).
#' @export
cohortGraph <- function(input, output, session,
                        cohgraph, cohdata) {

  ppColors <- c("Autoab Pos" = "orange", "Cystic fibrosis" = "aquamarine4", "Gastric Bypass" = "bisque4",
                "Gestational diabetes" = "deeppink2", "Monogenic Diabetes" = "red4", "No diabetes" = "royalblue2",
                "Other-Diabetes" = "indianred4", "Other-No Diabetes" = "steelblue2", "T1D" = "red",
                "T1D Medalist" = "maroon", "T2D" = "purple")
  output$cohGraph <- renderPlotly({
    cohgraph %>% config(displayModeBar = F)
  })

  output$cohPie <- renderPlotly({
    hover <- event_data("plotly_hover")
    if(is.null(hover$key)) return()
    if(hover$key == "NA") {
      pie <- cohdata[variable == hover$x & value == hover$y, table(donor.type)]
    } else {
      pie <- cohdata[variable == hover$key & value == hover$y, table(donor.type)]
    }
    pie <- melt(pie, measure.vars = names(pie))
    pie <- pie[order(pie$value), ]
    colors <- as.character(pie$donor.type[order(pie$value)])
    colors <- c(ppColors, "Pending" = "gray", "Pregnancy" = "pink", "Transplant" = "darkseagreen4")[colors]
    colors <- apply(col2rgb(colors), 2, function(x) paste0("rgb(", x[1], ",", x[2],",", x[3], ")"))
    p <- plot_ly(pie, labels = ~donor.type, values = ~value, type = 'pie', sort = F,
                 textinfo = "label", hole = 0.3, showlegend = F,
                 marker = list(colors = colors),
                 width = 250, height = 300,
                 insidetextfont = list(color = "#FFFFFF")) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor= "transparent", plot_bgcolor = "transparent",
             autosize = F, margin = list(t = 100, b = 100, r = 25, l = 80),
             font = list(size = 10))
    p
  })
}
