#' Shiny module UI for comparison histodot plots of attributes in two datasets
#'
#' Histodot plots of two datasets side-by-side, with or without match information
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param s1label Optional, label for first selection.
#' @param s2label Optional, label for second selection.
#' @param placeholder Optional, placeholder for selection menus.
#' @return Menu UI for selecting attributes from s1 and s2 datasets.
#' @family matchPlot functions
#' @export
matchPlotUI <- function(id, s1label = NULL, s2label = NULL, placeholder = "(select from attributes)") {
  ns <- NS(id)
  renderjs <- I("{ option: function(item, escape) {
                   return '<div class=\"unused covariate\">' + escape(item.value) + '</div><br>' }
                }")
  tags$div(class = "matchPlot-ui", id = ns("matchPlot-ui"),
    tags$div(id = ns("s2-select-container"), class = "input-panel",
             selectizeInput(ns("s2_select"), s2label, choices = "", selected = "",
                            options = list(placeholder = placeholder, render = renderjs))),
    tags$div(id = ns("s1-select-container"), class = "input-panel",
             selectizeInput(ns("s1_select"), s1label, choices = "", selected = "",
                            options = list(placeholder = placeholder, render = renderjs))),
    tags$div(plotOutput(ns("plot")))
  )
}

#' Shiny module server for comparison histodot plots of attributes in two datasets
#'
#' Histodot plots of two datasets side-by-side, with or without match information
#'
#' The module was implemented to compare two cohort datasets but can also allow comparison
#' of other types of datasets that are conceptually similiar.
#' For the original cohort matching use case, if there happens to be match return values
#' from \code{\link{matchResultServer}} passed to \code{results},
#' the plots will add color-coding to visualize which individuals are matched.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param s1data Reactive data from source #1 (often the reference dataset).
#' @param s2data Reactive data from source #2, such as from \code{\link{customDatasetServer}}.
#' @param results Optional, reactive return value of \code{\link{matchResultServer}}.
#' @param ignorev Optional, a character vector of variables such as IDs to exclude from selection for plotting.
#' @family matchPlot functions
#' @export
matchPlotServer <- function(id,
                            s1data, s2data,
                            results,
                            ignorev = NULL) {

  moduleServer(id, function(input, output, session) {


    observe({
      updateSelectizeInput(session, "s2_select", choices = c("", removeID(names(s2data()), c("ID", ignorev))) )
      updateSelectizeInput(session, "s1_select", choices = c("", removeID(names(s1data()), c("ID", ignorev))) )
    })

    renderjs <- I("{ option: function(item, escape) {
                       return item.optgroup == 'Matched'?
                       '<div class=\"used covariate\">' + escape(item.value) + '</div><br>'
                       : '<div class=\"unused covariate\">' + escape(item.value) + '</div><br>' }
                    }")

    # Update select menu to partition attributes that were used for matching from "Other" non-matching attributes
    observeEvent(results$params, {
      s1list <- list(Matched = names(results$params), Other = removeID(setdiff(names(s1data()), names(results$params)), c("ID", ignorev)) )
      s2list <- list(Matched = unname(results$params), Other = removeID(setdiff(names(s2data()), results$params), c("ID", ignorev)) )
      updateSelectizeInput(session, "s1_select", choices = s1list,
                           selected = character(0), server = T,
                           options = list(placeholder = "(data attributes)", render = renderjs))
      updateSelectizeInput(session, "s2_select", choices = s2list,
                           selected = character(0), server = T,
                           options = list(placeholder = "(data attributes)", render = renderjs))
    })

    # Plots
    s1Plot <- reactiveVal(NULL)
    s2Plot <- reactiveVal(NULL)

    # Appends column Matched (used for fill color) whenever results$matchtable is non-null
    s1_localdata <- reactive({
      data <- as.data.frame(s1data())
      if(!is.null(results$matchtable)) {
        matches <- results$matchtable$ID
        data$Matched <- ifelse(data$ID %in% matches, "matched", "un-matched")
      } else {
        data$Matched <- "un-matched"
      }
      data
    })

    s2_localdata <- reactive({
      data <- as.data.frame(s2data())
      if(!is.null(results$matchtable)) {
        matches <- results$matchtable$match.ID
        data$Matched <- ifelse(data$ID %in% matches, "matched", "un-matched")
      } else {
        data$Matched <- "un-matched"
      }
      data
    })

    plotMatched <- function(y) {
      data <- as.data.frame(results$intermediate)
      data$Matched <- ifelse(is.na(results$pair), "un-matched", "matched")
      p <- cohistPlot(data, y = y)
      s1Plot(p)
      s2Plot(NULL)
    }

    plotS1 <- function(data = s1_localdata(), y) {
      p <- cohistPlot(data = s1_localdata(), y = y)
      s1Plot(p)
    }

    plotS2 <- function(data = s2_localdata(), y) {
      p <- cohistPlot(data, y = y)
      s2Plot(p)
    }

    s1_obs <- observeEvent(input$s1_select, {
      # Plot on same scale automatically for matched attributes
      if(input$s1_select %in% names(results$params)) {
        updateSelectizeInput(session, "s2_select", selected = results$params[names(results$params) == input$s1_select])
        plotMatched(input$s1_select)
      } else {
        if(input$s1_select == "") return()
        plotS1(y = input$s1_select)
        if(input$s2_select != "") plotS2(y = input$s2_select)
      }
    })

    s2_obs <- observeEvent(input$s2_select, {
      if(input$s2_select %in% results$params) {
        y <- names(results$params)[input$s2_select == results$params]
        updateSelectizeInput(session, "s1_select", selected = y) # plotMatched will run under s1_obs
      } else {
        if(input$s2_select == "") return()
        plotS2(y = input$s2_select)
        if(input$s1_select != "") plotS1(y = input$s1_select)
      }
    })

    output$plot <- renderPlot({
      if(length(s1Plot()) && length(s2Plot())) {
        gridExtra::grid.arrange(grobs = list(s2Plot(), s1Plot()), ncols = 2, nrow = 1)
      } else if (length(s2Plot())) {
        s2Plot()
      } else {
        s1Plot()
      }
    })

  })

}

# -- Helpers ---------------------------------------------------------------------------------------------------------#

#' A custom comparison histodot plot
#'
#' @keywords internal
cohistPlot <- function(data, x = "Cohort", y, fill = "Matched") {
  # Need two separate geom_dotplot layers to ensure that matched points are always on top
  p <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_dotplot(data = data[data[[fill]] == "un-matched", ], method = "histodot", stackdir = "center", binaxis = "y") +
    geom_dotplot(data = data[data[[fill]] == "matched", ], method = "histodot", stackdir = "center", binaxis = "y") +
    scale_fill_manual(values = c(`matched` = "palegreen", `un-matched` = "gray")) +
    theme_bw(base_size = 20)
  p
}
