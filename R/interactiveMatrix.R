#' Shiny module UI of matrix plot with drilldown components
#'
#' Creates app UI for a heatmap matrix responding to inputs, new plot data, that is linked to a drilldown scatterplot component.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
interactiveMatrixUI <- function(id) {
  ns <- NS(id)
  tags$div(
    fluidRow(
    column(8, align = "left",
           div(id = "matrixOutput",
               shinycssloaders::withSpinner(uiOutput(ns("heatmap")), color = "gray"))
    ),
    column(4,
           div(id = "drilldownOutput",
               selectizeInput(ns("drilldown"), "Drill down to data for",
                              choices = "", selected = "",
                              options = list(maxItems = 2, placeholder = "select variable(s)"), width = "400px"),
               conditionalPanel(paste0("input['", ns("drilldown"), "']"),
                                div(class = "forceInline",
                                    selectInput(ns("colorby"), "Color data points by", choices = "", selected = "", width = "200px")),
                                div(class = "forceInline", br(), actionButton(ns("switchXY"), "XY", icon = icon("refresh"))),
                                div(class = "forceInline", br(), checkboxInput(ns("plotsmooth"), "Add trend")),
                                plotlyOutput(ns("scatter"))
               )
           )
    ))
  )
}

#' Server module server for plotting correlations matrix with drilldown interaction
#'
#' The matrix responds to inputs, new plotdata, and has a linked drilldown component.
#'
#' The module handles interactive display of a matrix linked to a scatter
#' plot that accesses underlying data for a user-clicked cell. It works with \code{\link{matrixCtrl}},
#' which returns the reactive matric as part of a \code{state} object. The general idea is that
#' this module only handles visualization of a matrix while the actual matrix generation can be handled by
#' other modules that feed into it. For the drilldown component that displays underlying data points,
#' the type of data matters and may require setting aesthetic parameters to maintain consistency across modules.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param state Reactive state object from \code{\link{matrixCtrl}}.
#' @param factorx Names and/or patterns for variables that should be displayed as factors in scatterplot.
#' @param dcolors Optional, a list of at least one categorical variable to use for scatterplot point colors
#' and the manual color mappings for each in the list. If color mappings are not specified, random colors will be used
#' and may not be consistent with other module plots.
#' @export
interactiveMatrix <- function(input, output, session,
                              state, factorx = "type$|grp$|cat$|score$|grade$|bin$|pos$|^male$",
                              dcolors = NULL)
  {

  #-- Main matrix plot -----------------------------------------------------------------------------------------------------#

  output$heatmap <- renderUI({
    if(nrow(state$filM)) plotlyOutput(session$ns("matrix")) else htmlOutput(session$ns("empty"))
  })

  output$matrix <- renderPlotly({

     M <- state$filM
     # bug? plotly doesn't display the plot if height is less than 100px
     # max height should be ~1000px
     px <- 1000/ncol(M)
     height <- nrow(M) * px
     height <- if(height < 200) 400 else height
     newdata <- state$newdata
     show <- nrow(M) <= 20 # only show y axis-labels when a reasonable number of rows is being displayed

     p <- plot_ly(x = colnames(M), y = rownames(M), z = M,
                  type = "heatmap", colorscale = "RdBu", source = "matrix", hovertemplate = "row: <b>%{x}</b><br>col: <b>%{y}</b><br>correlation: <b>%{z}</b>",
                  height = height, colorbar = list(thickness = 8)) %>%
       layout(xaxis = list(title = "", showgrid = F, showticklabels = FALSE, ticks = "", linecolor = "gray", mirror = T),
              yaxis = list(title = "", showgrid = F, showticklabels = show, ticks = "", tickfont = list(color = "gray"), linecolor = "gray", mirror = T),
              plot_bgcolor = "gray") %>%
       event_register("plotly_click") %>%
       config(displayModeBar = F)

     # group annotations for columns
     if(is.null(isolate(state$colmeta))) {
       colAnnot <- plotly_empty()
     } else {
       colAnnot <- plotly_empty()
     }

     # group annotations for rows
     if(is.null(isolate(state$rowmeta))) {
       rowAnnot <- plotly_empty()
      } else {
       rowAnnot <- plot_ly(x = 1, y = rownames(M), z = as.matrix(factor(state$rowmeta)))
      }

     subplot(plotly_empty(), colAnnot, p, rowAnnot,
             nrows = 2, shareX = T, shareY = T,
             widths = c(0.05, 0.95), heights = c(0.05, 0.95))
  })

  output$empty <- renderUI({
    "No meaningful results. Try expanding filters."
  })

  #-- Drilldown handling -----------------------------------------------------------------------------------------------------#
  observeEvent(state$cdata, {
    updateSelectizeInput(session, "drilldown", choices = removeID(names(state$cdata)), selected = character(0))
  })

  observe({
    s <- event_data("plotly_click", source = "matrix")
    if(is.null(s)) return()
    var1 <- s[["x"]]
    var2 <- s[["y"]]
    updateSelectizeInput(session, "drilldown", "Drill down to data for", selected = c(var1, var2))
  })

  output$scatter <- renderPlotly({
    req(!is.null(input$drilldown))
    var1 <- input$drilldown[1]
    var2 <- input$drilldown[2]
    tmp <- as.data.frame(state$cdata)
    dgroup <- names(dcolors) # default categorical group colors
    colorby <- dgroup
    if(grepl(factorx, var1)) tmp[[var1]] <- factor(tmp[[var1]])

    if(!is.na(var2)) { #-> do scatter plot 2-variable view -------------------------------------#
      tmp <- tmp[complete.cases(tmp[, c(var1, var2)]), ]
      if(grepl(factorx, var2)) tmp[[var2]] <- factor(tmp[[var2]])
      p <- ggplot(tmp, aes_string(x = var1, y = var2)) +
        labs(title = paste("n =", nrow(tmp))) +
        theme_bw()
      if(all(grepl(factorx, c(var1, var2)))) {
        p <- p + geom_count() # deals with overplotting when both variables are categorical
      } else {
        p <- p + geom_point(aes_string(color = colorby), size = 2, alpha = 0.7)
      }
      if(colorby == dgroup) { # default is to color points by dgroup
        p <- p + scale_colour_manual(values = dcolors[[1]])
      } else { # continuous color scale for interval variable
        p <- p + scale_colour_distiller(palette = "YlOrRd", na.value = "black")
      }
      if(input$plotsmooth) p <- p + stat_smooth(method = "lm")
      if(input$switchXY) p <- p + coord_flip()
      p <- ggplotly(p)
      p %>% config(displayModeBar = F)

    } else { #-> do boxplot 1-variable view ----------------------------------------------------#
      tmp <- tmp[!is.na(tmp[[var1]]), ]
      tmp[[dgroup]] <- factor(tmp[[dgroup]])
      p <- ggplot(tmp, aes_string(x = dgroup, y = var1)) +
        geom_boxplot(outlier.color = NA) +
        scale_colour_manual(values = dcolors[[1]]) +
        labs(title = paste("n =", nrow(tmp))) +
        theme_bw()
      if(is.factor(tmp[[var1]])) {
        p <- p + geom_count(aes_string(color = dgroup))
      } else {
        p <- p + geom_point(aes_string(color = dgroup), size = 2, alpha = 0.5,
                            position = position_jitter(width = 0.05, height = 0.05))
      }
      p <- ggplotly(p)
      p$x$data[[1]]$marker$opacity <- 0 # manual specify since plotly doesn't translate this for boxplot
      p <- hide_legend(p)
      if(length(levels(tmp[[dgroup]])) > 4) p <- p %>% layout(xaxis = list(tickangle = 45))
      p %>% config(displayModeBar = F)
    }
  })

}
