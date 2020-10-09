#' Shiny app UI for creating comparison plots between subgrouped data
#'
#' UI for user to define groups for volcano plots
#'
#' @family multiVApp module functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @import shiny
#' @export
subgroupVUI <- function(id) {

  ns <- NS(id)
  tags$div(id = ns("viewport"), class = "subgroups-panel",
           div(align = "right", actionButton(ns("remove"), "", icon = icon("times"))),
           uiOutput(ns("selecthd")),
           div(class = "ui-inline", style = "height: 100px", uiOutput(ns("select1"))),
           div(class = "ui-inline", style = "height: 100px", uiOutput(ns("by1"))),
           div(class = "ui-inline", style = "height: 100px", uiOutput(ns("select2"))),
           div(class = "ui-inline", style = "height: 100px", uiOutput(ns("by2"))),
           div(style = "padding-bottom: 20px;", actionButton(ns("go"), "View")),
           shinycssloaders::withSpinner(color = "gray", size = 0.5, plotly::plotlyOutput(ns("plot")))
  )
}

#' Shiny module server for creating comparison plots between subgrouped data
#'
#' This primarily allows creation of volcano plots for highthroughput datasets based on user-defined contrast groups
#'
#' @family multiVApp module functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param cdata A data.table of clinical, phenotype, or other experimental data used to define groups.
#' @param hdlist A list of named high-dimensional (high-throughput datasets)
#' @import shiny
#' @export
subgroupVServer <- function(id,
                            cdata, hdlist) {

  moduleServer(id, function(input, output, session) {

    # updateSelectizeInput(session, "hdataset0", choices = LETTERS[1:5], selected = NULL)
    plotOut <- reactiveVal(plotly::plotly_empty())

    output$selecthd <- renderUI({
      selectInput(session$ns("hdataset"), "HT dataset", choices = names(hdlist))
    })

    output$select1 <- renderUI({
      selectInput(session$ns("groupby1"), "(A) group by", choices = names(cdata))
    })

    output$select2 <- renderUI({
      selectInput(session$ns("groupby2"), "(B) group by", choices = names(cdata))
    })


    # When feature variable is selected, render appropriate UI used to define groups
    # and constrain choices to levels available in selected ht dataset
    output$by1 <- renderUI({
      if(is.null(input$groupby1)) return(NULL)
      x <- cdata[[input$groupby1]]
      if(class(x) == "character" | class(x) == "factor") {
        choices <- cdata[ID %in% rownames(hdlist[[input$hdataset]]), unique(get(input$groupby1))]
        tags$div(
          selectizeInput(session$ns("s1"), "factor level(s)", choices = choices, multiple = T, width = 200)
        )
      } else {
        x <- na.omit(cdata[ID %in% rownames(hdlist[[input$hdataset]]), get(input$groupby1)])
        if(length(x)) {
          sliderInput(session$ns("s1"), label = "range",
                      min = min(x), max = max(x), value = c(min(x), max(x)),
                      width = 200, ticks = F)
        }
      }
    })

    output$by2 <- renderUI({
      if(is.null(input$groupby2)) return(NULL)
      x <- cdata[[input$groupby2]]
      if(class(x) == "character" | class(x) == "factor") {
        choices <- cdata[ID %in% rownames(hdlist[[input$hdataset]]), unique(get(input$groupby2))]
        tags$div(
          selectizeInput(session$ns("s2"), label = "factor level(s)", choices = choices, multiple = T, width = 200)
        )
      } else {
        x <- na.omit(cdata[ID %in% rownames(hdlist[[input$hdataset]]), get(input$groupby2)])
        if(length(x)) {
          sliderInput(session$ns("s2"), label = "range",
                      min = min(x), max = max(x), value = c(min(x), max(x)),
                      width = 200, ticks = F)
        }
      }
    })

    # Get the nPOD IDs for each group defined
    group1 <- reactive({
      if(is.numeric(input$s1)) {
        intersect(cdata[which(findInterval(cdata[[input$groupby1]], input$s1, rightmost.closed = T) == 1), ID],
                  rownames(hdlist[[input$hdataset]]))
      } else {
        intersect(cdata[get(input$groupby1) %in% input$s1, ID], rownames(hdlist[[input$hdataset]]))
      }
    })

    group2 <- reactive({
      if(is.numeric(input$s2)) {
        intersect(cdata[which(findInterval(cdata[[input$groupby2]], input$s2,  rightmost.closed = T) == 1), ID],
                  rownames(hdlist[[input$hdataset]]))
      } else {
        intersect(cdata[get(input$groupby2) %in% input$s2, ID], rownames(hdlist[[input$hdataset]]))
      }
    })

    observeEvent(input$go, {
      # check for group size and whether groups overlap
      okgroupsize <- length(group1()) > 1 && length(group2()) > 1
      disjoint <- !length(intersect(group1(), group2()))
      if(!okgroupsize | !disjoint) {
        message <- if(!okgroupsize) "One or both groups contain fewer than two samples." else "Comparisons only allowed for disjoint groups."
        p <- plotly::plotly_empty()() %>%
          plotly::layout(title = message, font = list(color = "gray"))
      } else {
      # do fit, return data frame with (A) p-values, (B) fold difference, (C) color
        xm <- t(hdlist[[input$hdataset]])
        sampIDs <- colnames(xm)
        group <- factor(sampIDs)
        levels(group) <- list(g1 = group1(), g2 = group2())
        group <- addNA(group)
        levels(group)[3] <- "ignore"
        fit <- fitDesign(xm, group = group)
        fit2 <- fitContrast(fit, contrast = "g1-g2")
      # A
        adjP <- p.adjust(fit2$p.value, method = "fdr")
        neglogAdjP <- -log(adjP)
      # B
        means1 <- rowMeans(xm[, sampIDs %in% group1()])
        means2 <- rowMeans(xm[, sampIDs %in% group2()])
        diffx <- means1-means2
      # C
        sigcolor <- ifelse(adjP < 0.05, "significant", "not significant")
        p <- plot_ly(x = diffx, y = neglogAdjP, type = "scatter", mode = "markers",
                hoverinfo = "text", text = paste("<br>-log(adjusted p): ", neglogAdjP, "<br>Difference: ", diffx),
                color = sigcolor, colors = c(significant = "deeppink", `not significant` = "gray"),
                showlegend = T) %>%
          plotly::layout(xaxis = list(title = "Fold Change Difference [Group 1 - Group 2]"), yaxis = list(title = "-log(adjusted p-value)"),
                 legend = list(orientation = "h", y = 1.02, yanchor = "bottom"))
      }
      plotOut(p)
    })

    output$plot <- plotly::renderPlotly({
      plotOut() %>%
        plotly::config(displayModeBar = F)
    })

    observeEvent(input$remove, {
      removeUI(selector = paste0("#", session$ns("viewport")))
    }, once = T)

  })

}

fitDesign <- function(matrix, group) {
  design <- model.matrix(~0 + group)
  colnames(design) <- gsub("group", "", colnames(design))
  fit <- limma::lmFit(matrix, design)
  return(fit)
}

fitContrast <- function(fit, contrast) {
  cont.matrix <- limma::makeContrasts(contrasts = contrast, levels = fit$design)
  fit2 <- limma::contrasts.fit(fit, cont.matrix) %>% limma::eBayes()
  return(fit2)
}
