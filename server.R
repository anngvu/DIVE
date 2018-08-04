library(shiny)

#-- SET-UP:variables ------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#

shinyServer(function(input, output, session) {
   
#-- PAGE 1 ----------------------------------------------------------------------------------------#
  
  output$network <- renderVisNetwork({
    g %>% visIgraphLayout(randomSeed = 88) %>% visOptions(highlightNearest = TRUE)
  })
  
  observeEvent(input$accessdata, {
    updateNavbarPage(session, "main", selected = "source-data")
  })
  
#-- PAGE 2 ----------------------------------------------------------------------------------------#
  
  #  Correlations
  output$corM <- renderPlotly({
    #tmp <- plotdata$corr
    tmp <- read.table("Data/cdata2.txt")
    vars <- names(tmp)
    tmp <- round(cor(tmp, use = "pairwise.complete.obs"), 2)
    # Newly imported variables are labeled in red
    #labcolors <- ifelse(unique(tmp$Var1) %in% cdata.vars, "black", "red")
    #n <- input$n_minimum
    #tmp$value[tmp$n < n] <- NA  
    # p <- ggplot(tmp, aes(x = Var1, y = Var2, fill = value)) + geom_tile() +
    #   theme_classic() +
    #   theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = labcolors),
    #         axis.text.y = element_text(color = labcolors),
    #         panel.background = element_rect(fill = "gray")) +
    #         scale_fill_distiller(palette = "RdBu")
    # p <- ggplotly(p, width = 900, height = 750, source = "correlation")
    p <- plot_ly(x = vars, y = vars, z = tmp, key = tmp, 
                 type = "heatmap", source = "correlation", colorscale = "RdBu",
                 width = 900, height = 750) %>%
      layout(xaxis = list(title = "", showgrid = F), 
             yaxis = list(title = "", showgrid = F), plot_bgcolor = "lightgray")
    p
  })
  
  observeEvent(input$var_exclude, {
    corr <- plotdata$corr.last.state
    corr <- corr[!corr$Var1 %in% input$var_exclude, ]
    corr <- corr[!corr$Var2 %in% input$var_exclude, ]
    plotdata$corr <- corr
  })
  
  observeEvent(input$reset_vars, {
    plotdata$corr <- plotdata$corr.last.state
    updateSelectizeInput(session, "var_exclude", "Exclude variables from correlation matrix", selected = character(0))
  })
  
  fluidRow(
    column(1, 
           numericInput("n_minimum", HTML("min. N for <i>r</i>"), min = 2, max = NA, step = 1, val = 5)
    ),
    column(3, 
           selectizeInput("var_exclude", "Exclude variables from correlation matrix", cdata.vars, multiple = T)
    ),
    column(1, 
           br(),
           actionButton("reset_vars", "Reset")
    ),
    column(4, 
           div(class = "forceInline", 
               fileInput("newdata", "", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                         buttonLabel = "My data...", placeholder = "Upload data for comparison")),
           div(class = "forceInline", br(), actionButton("helpNew", "", icon = icon("question-circle")))
    ),
    column(3, 
           selectizeInput("drilldown", "Drill down to data points for V1 or V1 x V2:", 
                          choices = c("", unique(corM$Var1)), selected = "", options = list(maxItems = 2))
    ),
    column(12, align = "center",
           plotlyOutput("corM")
    )
  )
  
  # Help
  observeEvent(input$helpNew, {
    showModal(modalDialog(
      title = "Uploading data",
      HTML("<strong>What kind of data</strong><br>This works for simple data that can be formatted as a table. For high-throughput, image, or other complex data, please contact us or go to (OFFICIAL PROCESS TBD)<br><br><strong>Format</strong><br>The table should have a first column named 'ID' with nPOD case IDs, which we use as the key for joining data.<br><br><strong>Data confidentiality</strong><br>Note that this DOES NOT automatically save your data within our server. To officially contribute your data, go to (OFFICIAL PROCESS TBD)"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  

  observeEvent(input$newdata, {
    newdata <- fread(input$newdata$datapath, header = T)
    names(newdata) <- make.names(names(newdata))
    plotdata$newdata <- newdata
    newacc <- merge(cdata, newdata, by.x = "ID", by.y = names(newdata)[1], all = T)
    plotdata$acc <- newacc
    newcorrs <- melt(cor(newacc[, -c("ID", "donorType", "CR.gender", "CR.ethnic", "CR.COD", "CR.ABO")], use = "pairwise.complete.obs"))
    n <- melt(crossprod(as.matrix(newacc[, lapply(.SD, function(x) as.integer(!is.na(x))), .SDcols = !c("ID", "donorType", "CR.gender", "CR.ethnic", "CR.COD", "CR.ABO")])))
    newcorrs$n <- n$value
    plotdata$corr <- newcorrs
    plotdata$corr.last.state <- newcorrs
    updateSelectizeInput(session, "drilldown", "Drill down to data points for V1, or V1 x V2:", choices = c("", unique(newcorrs$Var1)),
                         selected = newcorrs$Var1[1], options = list(maxItems = 2))
    updateSelectizeInput(session, "var_exclude", "Exclude variables from correlation matrix", 
                         choices = names(newacc)[!names(newacc) %in% c("ID", "donorType", "CR.gender", "CR.ethnic", "CR.COD", "CR.ABO")])
    updateSelectInput(session, "colorby", "Color data points by", choices = names(newacc)[!names(newacc) %in% "ID"], selected = "donorType")
  })
  
  output$scatter <- renderPlotly({
    req(!is.null(input$drilldown))
    drilldown <- input$drilldown
    tmp <- as.data.frame(plotdata$acc)
    Var1 <- drilldown[1]
    if(length(drilldown) == 2) { # Scatter plot for 2-variable view
      Var2 <- drilldown[2]
      tmp <- tmp[complete.cases(tmp[, c(Var1, Var2)]), ]
      p <- ggplot(tmp, aes_string(x = Var1, y = Var2)) + 
        geom_point(aes_string(color = input$colorby), size = 2, position = position_jitter(width = 0.05, height = 0.05)) + 
        labs(title = paste0("n = ", nrow(tmp))) +
        theme_minimal()
      if(input$colorby == "donorType") {
        p <- p + scale_colour_manual(values = ppColors)
      } else if(input$colorby %in% fvars) {
        p <- p + scale_color_d3("category20")
      } else {
        p <- p + scale_colour_distiller(palette = "YlOrRd", na.value = "black")
      }
      if(input$plotsmooth) p <- p + stat_smooth(method = "lm")
      p <- ggplotly(p)
      p
    } else { # Boxplot for 1-variable view
      tmp <- tmp[!is.na(tmp[[Var1]]), ]
      tmp$donorType <- factor(tmp$donorType)
      p <- ggplot(tmp, aes_string(x = "donorType", y = Var1)) +
        geom_boxplot() + 
        geom_point(aes(fill = donorType), size = 2, position = position_jitter(width = 0.05, height = 0.05)) +
        scale_colour_manual(values = ppColors) +
        labs(title = paste0("n = ", nrow(tmp))) +
        theme_minimal()
      p <- ggplotly(p)
      p
    }  
  })
  
  observe({
    s <- event_data("plotly_click", source = "correlation")
    if(length(s)) {
      Var1 <- s[["x"]]
      Var2 <- s[["y"]]
      updateSelectizeInput(session, "drilldown", "Drill down to data points for V1, or V1 x V2:", selected = c(Var1, Var2))
    } else {
      return(NULL)
    }
  })
  
#-- PAGE 3 ----------------------------------------------------------------------------------------#
  
  updateSelectizeInput(session, "Glist", "Custom gene list", choices = names(GENES), 
                       selected = character(0), options = list(maxItems = 20), server = T)
  
  output$volcanos <- renderPlotly({
    .gx <- gx
    .px1 <- px1
    .px2 <- px2
    a.gx.T1D <- a.px1 <- a.px2.T1D <- a.px2.AAB <- NULL
    if(length(input$Glist)) { 
        genes <- GENES[input$Glist]
        # There's a bug in Plotly that prevents using this option:
        # a.gx.T1D <- gx[Entrez %in% genes, .(Diff.T1DvsHC, nlogP.T1DvsHC, Gene)]
        # a.gx.T1D <- list(x = a.gx.T1D$Diff.T1DvsHC, y = a.gx.T1D$nlogP.T1DvsHC, xref = "x", yref = "y", 
        #                  text = a.gx.T1D$Gene, showarrow = T, arrowhead = 7)
        # 
        # a.px1 <- px1[Entrez %in% genes | Entrez.2 %in% genes, .(Difference, nlogP, Protein)]
        # a.px1 <- list(x = a.px1$Difference, y = a.px1$nlogP, xref = "x1", yref = "y1",
        #               text = a.px1$Protein, showarrow = T, arrowhead = 7)
        #   
        # a.px2.T1D <- px2[Entrez %in% genes, .(Diff.T1DvsHC, nlogP.T1DvsHC, Protein)]
        # a.px2.T1D <- list(x = a.px2.T1D$Diff.T1DvsHC, y = a.px2.T1D$nlogP.T1DvsHC, xref = "x2", yref = "y2",
        #                   text = a.px2.T1D$Protein, showarrow = T, arrowhead = 7)
        # 
        # a.px2.AAB <- px2[Entrez %in% genes, .(Diff.AABvsHC, nlogP.AABvsHC, Protein)]
        # a.px2.AAB <- list(x = a.px2.AAB$Diff.AABvsHC, y = a.px2.AAB$nlogP.AABvsHC, xref = "x3", yref = "y3",
        #                   text = a.px2.AAB$Protein, showarrow = T, arrowhead = 7)
        
      .gx[, Highlight := Entrez %in% genes]
      .gx[Highlight == T, Label := "X"]
      .px1[, Highlight := Entrez %in% genes | Entrez.2 %in% genes]
      .px1[Highlight == T, Label := "X"]
      .px2[, Highlight := Entrez %in% genes]
      .px2[Highlight == T, Label := "X"]
      pal <- c("#A69EB0FF", "#00FF83FF", "yellow2", "darkorchid1", "royalblue1", "violetred1", "black")
    } else {
      .gx[, Highlight := F]
      .px1[, Highlight := F]
      .px2[, Highlight := F]
    }
    
    gx.T1D <- plot_ly(data = .gx, x = ~Diff.T1DvsHC, y = ~nlogP.T1DvsHC, text = ~Gene, type="scatter", mode = "markers",
                  color = ~factor(Label), symbol = ~Highlight, symbols = c("circle", "square"), colors = pal[c(1,3,6)], showlegend = FALSE) %>%
                  layout(annotations = a.gx.T1D)
    px1 <- plot_ly(data = .px1, x = ~Difference, y = ~nlogP, text = ~Protein, type="scatter", mode = "markers",
                   color = ~factor(Label), colors = pal[c(1,3,6)], showlegend = FALSE) %>%
                   layout(annotations = a.px1)
    px2.T1D <- plot_ly(data = .px2, x = ~Diff.T1DvsHC, y = ~nlogP.T1DvsHC, text = ~Protein, type="scatter", mode = "markers",
                       color = ~Label, colors = pal, legendgroup = ~Label, showlegend = FALSE) %>%
                       layout(annotations = a.px2.T1D)
    px2.AAB <- plot_ly(data = .px2, x = ~Diff.AABvsHC, y = ~nlogP.AABvsHC, text = ~Protein, type="scatter", mode = "markers",
                       color = ~Label, colors = pal, legendgroup = ~Label) %>%
                       layout(plot_bgcolor = "#F7F7F7FF", legend = list(orientation = 'h', bgcolor = "#F7F7F7FF"))
    p <- subplot(gx.T1D, px1, px2.T1D, px2.AAB) 
    if(F) {
      gx.AAB <- plot_ly(data = .gx, x = ~Diff.AABvsHC, y = ~(nlogP.unadj.AABvsHC), text = ~Gene, type="scatter", mode = "markers", 
                        color = pal[1], showlegend = FALSE) %>%
        layout(title ="Volcano Plot")
      p <- subplot(gx.AAB, gx.T1D, px1, px2.T1D, px2.AAB, shareX = F, shareY = F) 
    }
    p
  })


#-- PAGE 5 ----------------------------------------------------------------------------------------#
  
  output$sourceDT <- DT::renderDataTable({
    show <- Columns[, .(Source, Contributors, IndividualLevelData, MPOLabel, Variable, Description, Relevance, Method, Note, DataSource, DataSourceLink)]
    if(input$filterDT) {
      show <- show[IndividualLevelData == "Yes", ]
    }
    show[IndividualLevelData == "Yes", DataSourceLink := paste0("<a href='",DataSourceLink,"' target='_blank'"," title='",DataSourceLink,"'>Get from original source","</a>")]
    show[IndividualLevelData != "Yes", DataSourceLink := ""]
    setnames(show, c("MPOLabel", "IndividualLevelData", "DataSource", "DataSourceLink"), 
             c("Theme or Area of Interest", "Individual-Level Data", "Source", "Data Source Link"))
  }, escape = FALSE, rownames = F, options = list(dom = 'ftp', pageLength = 7))
  
})
