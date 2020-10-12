#' Shiny module UI for data drilldown interaction
#'
#' Creates app UI for a drilldown plot component
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @import shiny
#' @export
dualDrilldownUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "drilldown-ui", id = ns("drilldown-ui"),
           tags$script(sprintf('$("#%s").draggable({ start: function(event, ui) {
                                     console.log("moving to:" + event.pageY + " " + event.pageX);
                                     $(this).css({ position: "absolute", top: event.pageY + "px", left: event.pageX + "px"});
                                     window.dispatchEvent(new Event("resize"));
                                    }
                                   });', ns("drilldown-output"))),
           selectizeInput(ns("drilldown"), "Drill down to data for", width = "500px",
                          choices = "", selected = "",
                          options = list(maxItems = 2, placeholder = "select variable(s)")),
           conditionalPanel("input.drilldown != ''", ns = ns,
                            div(class = "ui-inline", br(), actionLink(ns("flipxy"), "flip XY", icon = icon("refresh"))),
                            plotly::plotlyOutput(ns("scatter")))
  )
}

#' Server module server for data drilldown interaction
#'
#' Drilldown component with two different modes of disply and linked up to two different inputs
#'
#' The drilldown component is, at its simplest, a \code{selectInput} through which
#' variables can be selected for adaptive visualization as a boxplot or scatterplot output
#' (depending on whether one or two variables are selected, respectively).
#' However, optional complexity can be added to the component by making it listen to
#' two other sources -- there can be two independent higher-level components
#' that can update this component's \code{selectInput},
#' analogous to the poor drilldown having two bosses telling it what data they want reported.
#' Thus the "dual" describes both how the drilldown renders in two different ways
#' as well as being able to be controlled by up to two different sources.
#' For this component the data requires a default factor grouping variable
#' to be passed into the aesthetics. If there were no default factor grouping variable,
#' when only one variable is selected we would not be able to generate the boxplot view.
#' It should also be possible to specify multiple variables able to be used for grouping
#' (which the user can switch between), but this isn't implemented yet.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param cdata A data.table. This is a reactive to allow the component to be updated with a new table of data.
#' @param colorby A named list where name matches the (factor) variable in cdata to use for color grouping.
#' If there is a named vector, this is passed into scale_color_manual to be used for custom color mapping, e.g.
#' `list(MyGroupingVar = c(A = "red", B = "blue"))` (useful for enforcing a consistent/meaningful color scheme),
#' or `list(MyGroupingVar = NULL)` if custom color scaling isn't necessary. See also details.
#' @param factorx Optional, a function that returns a boolean for whether
#' a variable should be plotted as factor when given the variable name.
#' Useful when for some reason factor variables are numeric or character instead of already factor-encoded.
#' By default, variables are merely checked using `base::is.factor`, which works when the data is already factor-encoded.
#' @param src1 Reactive data from "source 1".
#' @param src2 Reactive data from "source 2".
#' @import shiny
#' @import data.table
#' @import ggplot2
#' @import magrittr
#' @export
dualDrilldownServer <- function(id,
                                cdata,
                                colorby,
                                factorx = function(x) is.factor(cdata[[x]]),
                                src1 = reactive(NULL), src2 = reactive(NULL)
                                ) {

  moduleServer(id, function(input, output, session) {

    observeEvent(cdata, {
      updateSelectizeInput(session, "drilldown", choices = names(cdata), selected = character(0))
    })

    # -- Main plot funs ----------------------------------------------------------------------------------------------------#

    # Single-variable boxplot by default factor group
    drillplot1 <- function(datasubset, var1, colorby, factorx) {
      p <- ggplot(datasubset, aes_string(x = names(colorby), y = var1)) +
        geom_boxplot(outlier.color = NA) +
        labs(title = paste("n =", nrow(datasubset))) +
        theme_bw()
      if(length(colorby[[1]])) p <- p + scale_colour_manual(values = colorby[[1]])
      if(factorx(var1)) {
        p <- p + geom_count(aes_string(color = names(colorby)))
      } else {
        p <- p + geom_point(aes_string(color = names(colorby)), size = 2, alpha = 0.5,
                            position = position_jitter(width = 0.05, height = 0.05))
      }
      p <- suppressWarnings(plotly::ggplotly(p)) %>%
        plotly::hide_legend() %>%
        plotly::layout(xaxis = list(tickangle = 45)) %>%
        plotly::config(displayModeBar = F)
      p$x$data[[1]]$marker$opacity <- 0 # manual specify since plotly doesn't translate this for boxplot
      p
    }

    # Two-variable scatterplot
    drillplot2 <- function(datasubset, var1, var2, colorby, factorx, flipxy) {
      p <- ggplot(datasubset, aes_string(x = var1, y = var2)) +
        labs(title = paste("n =", nrow(datasubset))) +
        theme_bw()
      # different plot when both variables are categorical
      if(factorx(var1) && factorx(var2)) {
        p <- p + geom_count()
      } else {
        p <- p + geom_point(aes_string(color = names(colorby)), size = 2, alpha = 0.7)
        if(length(colorby[[1]])) p <- p + scale_colour_manual(values = colorby[[1]])
      }
      if(flipxy) p <- p + coord_flip()
      p <- suppressWarnings(plotly::ggplotly(p)) %>% plotly::config(displayModeBar = F)
      p
    }

    #-- Drilldown handling -----------------------------------------------------------------------------------------------------#

    observeEvent(src1(), {
      updateSelectizeInput(session, "drilldown", selected = src1())
    }, ignoreInit = TRUE)

    observeEvent(src2(), {
      updateSelectizeInput(session, "drilldown", selected = src2())
    }, ignoreInit = TRUE)

    output$scatter <- plotly::renderPlotly({
      req(input$drilldown != "")
      var1 <- input$drilldown[1]
      colorgroup <- names(colorby)[1]

      if(length(input$drilldown) == 2) { #-> do scatter plot 2-variable view ----------------------#
        var2 <- input$drilldown[2]
        datasubset <- cdata[, c(..var1, ..var2, ..colorgroup)]
        datasubset <- datasubset[stats::complete.cases(datasubset)]
        drillplot2(datasubset, var1, var2, colorby[colorgroup], factorx, input$flipxy %% 2)

      } else { #-> do boxplot 1-variable view ----------------------------------------------------#
        datasubset <- cdata[!is.na(get(var1)), c(..var1, ..colorgroup)]
        drillplot1(datasubset, var1, colorby[colorgroup], factorx)
      }
    })

  })
}




