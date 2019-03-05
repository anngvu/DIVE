#' Shiny module UI for attribute-by-attribute comparison of two datasets
#'
#' Allows comparison of two datasets (cohorts, in the original case).
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param s1Label Optional, name or title of "source 1" dataset to display.
#' @param s1Data The full (non-reactive) "source 1" dataset, considered the reference dataset.
#' @param s2Label Optional, name or title of "source 2" dataset to display.
#' @param s2Data Reactive "source 2" data that will be compared to the first dataset, usually coming from newDatasetInput.
#' @param placeholder Optional, placeholder for attribute selection menus.
#' @return Menu UI for selecting attributes from s1 and s2 datasets.
#' @export
exploreMoreUI <- function(id, s1Label = "", s1Data, s2Label = "", s2Data, placeholder = "") {
  ns <- NS(id)
  displayjs <- I("{
                   option: function(item, escape) {
                   return '<div class=\"unused covariate\">' + escape(item.value) + '</div><br>'
                   }
                }")
  tags$div(id = "exploreMoreUI",
    fluidRow(
      column(6, div(id = "s2Attributes",
                    h4(s2Label),
                    selectizeInput(ns("s2Attrs"), "", choices = c("", removeID(names(s2Data()))), selected = character(0),
                                   options = list(placeholder = placeholder, render = displayjs))
      )),
      column(6, div(id = "s1Attributes",
                    h4(s1Label),
                    selectizeInput(ns("s1Attrs"), "", choices = c("", removeID(names(s1Data))), selected = character(0),
                                   options = list(placeholder = placeholder,
                                   render = displayjs))
      ))
    ),
    fluidRow(align = "center",
     plotOutput(ns("plot"), width = "90%")
    )
  )
}

#' Shiny module server for generating comparative histodot plots of attributes in two datasets
#'
#' Compare the composition between two datasets. In the cohort matching use case, if there is match data,
#' the plots show which subsets are matched and provides a means of visualizing the match results.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param s1Data The full (non-reactive) "source 1" dataset, considered the reference dataset.
#' @param s2Data Reactive "source 2" data that will be compared to the first dataset, usually coming from newDatasetInput.
#' @param results The reactive values return of matchResult.
#' @param factorx Suffixes used for grep to recognize plot variables as factors.
#' @return Histodot plots for comparing composition and matches between s1Data and s2Data.
#' @export
exploreMore <- function(input, output, session,
                       s1Data, s2Data, results,
                       factorx = "grp$|cat$|score$|bin$|count$|pos$|risk$") {

  # Update select menu to emphasize attributes that were used for matching
  observeEvent(results$params, {
    refli <- list(Matched = names(results$params), Other = removeID(setdiff(names(s1Data), names(results$params))))
    extli <- list(Matched = unname(results$params), Other = removeID(setdiff(names(s2Data()), results$params)))
    displayjs <- I("{
                    option: function(item, escape) {
                    return item.optgroup == 'Matched'?
                    '<div class=\"used covariate\">' + escape(item.value) + '</div><br>'
                    : '<div class=\"unused covariate\">' + escape(item.value) + '</div><br>'
                    }
                  }")
    updateSelectizeInput(session, "s1Attrs", "", choices = refli,
                         selected = character(0), server = T,
                         options = list(placeholder = "(cohort data attributes)",
                                        render = displayjs))
    updateSelectizeInput(session, "s2Attrs", "", choices = extli,
                         selected = character(0), server = T,
                         options = list(placeholder = "(cohort data attributes)", render = displayjs))
  })

  # Plotting
  s1Plot <- reactiveValues(data = NULL, p = NULL)
  s2Plot <- reactiveValues(data = NULL, p = NULL)

  plotMatched <- function(y) {
    data <- as.data.frame(results$intermediate)
    data$Matched <- factor(ifelse(is.na(results$pair), "un-matched", "matched"), levels = c("un-matched", "matched"))
    p <- cohistPlot(data, y = y, factorx = factorx)
    s1Plot$data <- data
    s1Plot$p <- p
    s2Plot$data <- s2Plot$p <- NULL
  }

  plotS1 <- function(y) {
    data <- copy(s1Data)
    data[, Cohort := "nPOD"] # To do: fix this to make more generalizable
    matches <- as.numeric(results$matchtable$ID)
    data$Matched <- factor(ifelse(data$ID %in% matches, "matched", "un-matched"), levels = c("un-matched", "matched"))
    p <- cohistPlot(data, y = y, factorx = factorx)
    s1Plot$data <- data
    s1Plot$p <- p
  }

  plotS2 <- function(y) {
    data <- as.data.frame(s2Data())
    matches <- results$matchtable$match.ID
    data$Matched <- factor(ifelse(data$ID %in% matches, "matched", "un-matched"), levels = c("un-matched", "matched"))
    p <- cohistPlot(data, y = y, factorx = factorx)
    s2Plot$data <- data
    s2Plot$p <- p
  }

  observeEvent(input$s1Attrs, {
    # Plot on same scale automatically for matched attributes
    if(input$s1Attrs %in% names(results$params)) {
      plotMatched(input$s1Attrs)
    } else {
      if(input$s1Attrs == "") return()
      plotS1(input$s1Attrs)
      if(input$s2Attrs != "") plotS2(input$s2Attrs)
    }
  })

  observeEvent(input$s2Attrs, {
    if(input$s2Attrs %in% results$params) {
      y <- names(results$params)[input$s2Attrs == results$params]
      plotMatched(y)
    } else {
      if(input$s2Attrs == "") return()
      plotS2(input$s2Attrs)
      if(input$s1Attrs != "") plotS1(input$s1Attrs)
    }
  })

  output$plot <- renderPlot({
    if(length(s1Plot$p) & length(s2Plot$p)) {
      gridExtra::grid.arrange(grobs = list(s2Plot$p, s1Plot$p), ncols = 2, nrow = 1)
    } else if (length(s2Plot$p)) {
      s2Plot$p
    } else {
      s1Plot$p
    }
  })

}

# -- Helpers -- -------------------------------------------------------------------------------------------------------#

removeID <- function(x) Filter(function(f) f != "ID", x)

# A custom cohort histodot plot
cohistPlot <- function(data, x = "Cohort", y, fill = "Matched", factorx) {
  if(grepl(factorx, y)) data[[y]] <- factor(data[[y]])
  p <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_dotplot(method = "histodot", stackdir = "center", binaxis = "y") +
    scale_fill_manual(values = c(`un-matched` = "gray", `matched` = "palegreen")) +
    theme_bw()
  p
}
