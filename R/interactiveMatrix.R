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
           div(style = "margin-left: 20px; margin-bottom: 10px;",
               div(class = "forceInline", actionButton(ns("show"), label = NULL, icon = icon("cog"))),
               conditionalPanel(condition = "input.show%2==1", ns = ns, class = "forceInline",
                                div(class = "forceInline", id = ns("custom")),
                                div(class = "forceInline", actionLink(ns("cluster"), "CLUSTER", icon = icon("sitemap"))),
                                div(class = "forceInline", actionLink(ns("showrlabs"), "+/- Row Labels")),
                                div(class = "forceInline", actionLink(ns("showclabs"), "+/- Col Labels")),
                                div(class = "forceInline", actionLink(ns("abspalette"), "Sign-agnostic", icon = icon("palette")))
               )
           ),
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
     height <- if(height < 400) 400 else ifelse(height > 1000, 1000, height)
     newdata <- state$newdata
     show <- nrow(M) <= 20 # only show y axis-labels when a reasonable number of rows is being displayed
     colorz <- if(input$abspalette %% 2) {
       colorRampPalette(c("#E74F00", "#FFE168", "gray", "#404040", "gray", "#FFE168", "#E74F00"))
     } else {
       colorRampPalette(c("#E74F00", "#FF894C", "gray", "#404040", "gray", "#4CC2FF", "#0098E7"))
     }
     # colorz <- colorRampPalette(c("#FF008C", "gray", "#404040", "gray", "#00FF73")) # not colorblind friendly
     p <- plot_ly(x = colnames(M), y = rownames(M), z = M, type = "heatmap", colors = colorz(101), name = "Exploratory\nMap",
                  hovertemplate = "row: <b>%{y}</b><br>col: <b>%{x}</b><br>correlation: <b>%{z}</b>", # xgap = 1, ygap = 1,
                  height = height, colorbar = list(thickness = 8)) %>%
       layout(xaxis = list(title = "", showgrid = F, showticklabels = input$showclabs %% 2 == 1, ticks = "", tickfont = list(color = "gray"), linecolor = "gray", mirror = T),
              yaxis = list(title = "", showgrid = F, showticklabels = input$showrlabs %% 2 == 1, ticks = "", tickfont = list(color = "gray"), linecolor = "gray", mirror = T),
              plot_bgcolor = "#404040") %>%
       event_register("plotly_click")

     # since plot_bgcolor can't be set for individual plots in subplot, layout adjusts to whether metadata is passed in
     if(!is.null(state$rowmeta)) {

       rvals <- scales::rescale(as.integer(state$rowmeta))
       cvals <- scales::rescale(as.integer(state$colmeta))

       if(state$optrowgroup == state$optcolgroup) {
         rdomain <- cdomain <- c(min(rvals, cvals, na.rm = T), max(rvals, cvals, na.rm = T)) # consistent colors for when annotations are the same type
         rpal <- cpal <- "Spectral"
       } else {
         rdomain <- cdomain <- NULL
         cpal <- "Spectral"
         rpal <- "Set3"
       }
       rowcolors <- scales::col_numeric(rpal, domain = rdomain)(rvals)
       rcolorscale <- data.frame(rvals, rowcolors)

       colcolors <- scales::col_numeric(cpal, domain = cdomain)(cvals)
       ccolorscale <- data.frame(cvals, colcolors)

       rtext <- matrix(as.character(state$rowmeta))
       rowmeta <- plot_ly(y = rownames(M), x = 1, z = matrix(as.integer(state$rowmeta)), text = rtext,
                          type = "heatmap", showscale = FALSE, name = "Row Group",
                          colorscale = "Portland", hovertemplate = "<b>%{text}</b>") %>%
         layout(xaxis = list(title = "", showgrid = F, showticklabels = F, ticks = ""),
                yaxis = list(title = "", showgrid = F, showticklabels = F, ticks = ""))

       ctext <- matrix(as.character(state$colmeta), nrow = 1)
       colmeta <- plot_ly(x = colnames(M), y = 1,  z = matrix(as.integer(state$colmeta), nrow = 1),
                          type = "heatmap", showscale = FALSE, name = "Column Group", text = ctext,
                          colorscale = "Portland", hovertemplate = "<b>%{text}</b>")  %>%
         layout(xaxis = list(title = "", showgrid = F, showticklabels = F, ticks = ""),
                yaxis = list(title = "", showgrid = F, showticklabels = F, ticks = ""))

       main <- subplot(colmeta, plotly_empty(), p, rowmeta,
                       nrows = 2, shareX = T, shareY = T, margin = 0.01,
                       widths = c(0.97, 0.03), heights = c(0.03, 0.97))
     } else {
       main <- p
     }
     main$x$source <- "main"
     main %>% plotly::config(displayModeBar = F)
  })

  output$empty <- renderUI({
    "No meaningful results. Try expanding filters."
  })

  #-- Drilldown handling -----------------------------------------------------------------------------------------------------#
  observeEvent(state$cdata, {
    updateSelectizeInput(session, "drilldown", choices = removeID(names(state$cdata)), selected = character(0))
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
      p %>% plotly::config(displayModeBar = F)

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
      p %>% plotly::config(displayModeBar = F)
    }
  })

}
