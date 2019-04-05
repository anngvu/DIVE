#' Shiny app UI for creating comparison plots between subgrouped data
#'
#' UI for user to define groups for volcano plots.
#'
#' @family multiVApp module functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param hdchoices List of high throughput datasets.
#' @param cchoices Data that can be used to define groups.
#' @export
subgroupVUI <- function(id, hdchoices, cchoices) {

  ns <- NS(id)
  tags$div(id = ns("viewport"), class = "subgroups-panel",
           div(align = "right", actionButton(ns("remove"), "", icon = icon("times"))),
           # verbatimTextOutput(ns("test")),
           selectInput(ns("hdataset"), "HT dataset", choices = hdchoices),
           div(class = "forceInline", style = "height: 100px", selectInput(ns("groupby1"), "(A) group by", choices = removeID(cchoices))),
           div(class = "forceInline", style = "height: 100px", uiOutput(ns("by1"), inline = T)),
           div(class = "forceInline", style = "height: 100px", selectInput(ns("groupby2"), "(B) group by", choices = removeID(cchoices))),
           div(class = "forceInline", style = "height: 100px", uiOutput(ns("by2"), inline = T)),
           div(style = "padding-bottom: 20px;", actionButton(ns("go"), "View")),
           shinycssloaders::withSpinner(color = "gray", size = 0.5, plotlyOutput(ns("plot")))
  )
}

#' Shiny module server for creating comparison plots between subgrouped data
#'
#' This primarily allows creation of volcano plots for highthroughput datasets based on user-defined group contrasts.
#'
#' @family multiVApp module functions
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param cdata A data.table of clinical, phenotype, or other experimental data used to define groups.
#' @param hdata A list of named high-dimensional (high-throughput datasets)
#' @export
subgroupV <- function(input, output, session,
                      cdata, hdata) {

  # updateSelectizeInput(session, "hdataset0", choices = LETTERS[1:5], selected = NULL)
  plotOut <- reactiveVal(plotly_empty())

  # When feature variable is selected, render appropriate UI for values used to define groups
  output$by1 <- renderUI({
    if(is.null(input$groupby1)) return(NULL)
    x <- cdata[[input$groupby1]]
    if(class(x) == "character" | class(x) == "factor") {
      tags$div(
        selectizeInput(session$ns("s1"), "factor level(s)", choices = unique(x), multiple = T, width = 200)
      )
    } else {
      x <- x[!is.na(x)]
      if(length(x)) {
        sliderInput(session$ns("s1"), label = "range", min = min(x), max = max(x), value = c(min(x), max(x)), width = 200)
      }
    }
  })

  output$by2 <- renderUI({
    if(is.null(input$groupby2)) return(NULL)
    x <- cdata[[input$groupby2]]
    if(class(x) == "character" | class(x) == "factor") {
      tags$div(
        selectizeInput(session$ns("s2"), label = "factor level(s)", choices = unique(x), multiple = T, width = 200)
      )
    } else {
      x <- x[!is.na(x)]
      if(length(x)) {
        sliderInput(session$ns("s2"), label = "range", min = min(x), max = max(x), value = c(min(x), max(x)), width = 200)
      }
    }
  })

  # Get the nPOD IDs for each group defined
  group1 <- reactive({
    if(is.numeric(input$s1)) cdata[which(findInterval(cdata[[input$groupby1]], input$s1, rightmost.closed = T) == 1), ID]
    else cdata[get(input$groupby1) %in% input$s1, ID]
  })

  group2 <- reactive({
    if(is.numeric(input$s2)) cdata[which(findInterval(cdata[[input$groupby2]], input$s2,  rightmost.closed = T) == 1), ID]
    else cdata[get(input$groupby2) %in% input$s2, ID]
  })

  observeEvent(input$go, {
    # check for group size and whether groups overlap
    okgroupsize <- length(group1()) > 1 && length(group2()) > 1
    disjoint <- !length(intersect(group1(), group2()))
    if(!okgroupsize | !disjoint) {
      message <- if(!okgroupsize) "Groups unavailable or contain fewer than two samples." else "Currently comparisons are only allowed for disjoint groups."
      p <- plotly_empty() %>%
        layout(title = message, font = list(color = "gray"))
    } else {
    # do fit, return data frame with (A) p-values, (B) fold difference, (C) color
      xm <- t(hdata[[input$hdataset]])
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
              showlegend = FALSE) %>%
        layout(xaxis = list(title = "Fold Change Difference [Group 1 - Group 2]"), yaxis = list(title = "-log(adjusted p-value)"))
    }
    plotOut(p)
  })

  output$plot <- renderPlotly({
    plotOut() %>%
      config(displayModeBar = F)
  })

  observeEvent(input$remove, {
    removeUI(selector = paste0("#", session$ns("viewport")))
  }, once = T)

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
