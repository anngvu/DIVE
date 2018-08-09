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
  
  # Help events -------------------------------------------------
  # Intro.js demo
  
  
  # -------------------------------------------------------------
  
#-- PAGE 2 ----------------------------------------------------------------------------------------#
  
  # Help events -------------------------------------------------
  observeEvent(input$helpUpload, {
    showModal(modalDialog(
      title = "Uploading data",
      includeHTML("uploading_data.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$helpCorrelation, {
    showModal(modalDialog(
      title = "Exploring correlations with old and new data",
      HTML(""),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # -------------------------------------------------------------
  
  #  Correlations
  output$corM <- renderPlotly({
    #tmp <- plotdata$corr
    tmp <- read.table("Data/cdata2.txt")
    vars <- names(tmp)
    tmp <- round(cor(tmp, use = "pairwise.complete.obs"), 2)
    # Newly imported variables are labeled in red
    #labcolors <- ifelse(unique(tmp$Var1) %in% cdata.vars, "black", "red")
    #n <- input$minimumN
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
  
  observeEvent(input$varExclude, {
    corr <- plotdata$corr.last.state
    corr <- corr[!corr$Var1 %in% input$varExclude, ]
    corr <- corr[!corr$Var2 %in% input$varExclude, ]
    plotdata$corr <- corr
  })
  
  observeEvent(input$varReset, {
    plotdata$corr <- plotdata$corr.last.state
    updateSelectizeInput(session, "varExclude", "Exclude variables from correlation matrix", selected = character(0))
  })

  observeEvent(input$dataUpload, {
    dataUpload <- fread(input$dataUpload$datapath, header = T)
    names(dataUpload) <- make.names(names(dataUpload))
    plotdata$dataUpload <- dataUpload
    newacc <- merge(cdata, dataUpload, by.x = "ID", by.y = names(dataUpload)[1], all = T)
    plotdata$acc <- newacc
    newcorrs <- melt(cor(newacc[, -c("ID", "donorType", "CR.gender", "CR.ethnic", "CR.COD", "CR.ABO")], use = "pairwise.complete.obs"))
    n <- melt(crossprod(as.matrix(newacc[, lapply(.SD, function(x) as.integer(!is.na(x))), .SDcols = !c("ID", "donorType", "CR.gender", "CR.ethnic", "CR.COD", "CR.ABO")])))
    newcorrs$n <- n$value
    plotdata$corr <- newcorrs
    plotdata$corr.last.state <- newcorrs
    updateSelectizeInput(session, "drilldown", "Drill down to data points for V1, or V1 x V2:", choices = c("", unique(newcorrs$Var1)),
                         selected = newcorrs$Var1[1], options = list(maxItems = 2))
    updateSelectizeInput(session, "varExclude", "Exclude variables from correlation matrix", 
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
  
  # Help events -------------------------------------------------
  observeEvent(input$helpVolcano, {
    showModal(modalDialog(
      title = "Volcano help",
      HTML(""),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # -------------------------------------------------------------
  
  # Update genes of interest drop-down
  updateSelectizeInput(session, "Glist", "Genes (proteins) of interest", choices = names(GENES), 
                       selected = character(0), options = list(maxItems = 20), server = T)
  
  # Update GO/Reactome drop-down
  observeEvent(input$GorR, {
    branch <- switch(input$BPCCMP,
      BP = setNames(GO.BP$GO, paste0(GO.BP$GOTerm, " (", GO.BP$Count, ")")),
      MF = "",
      CC = ""
    )
    if(input$GorR == "Gene Ontology") {
      updateSelectizeInput(session, "GOReactq", "GO term", choices = branch, selected = character(0), server = TRUE)
    } else {
      updateSelectizeInput(session, "GOReactq", "Reactome pathway", choices = names(PATHS), selected = character(0), server = TRUE)
    }
  })
  
  # Volcano render
  output$volcanos <- renderPlotly({
    pal2 <- pal[c(1,3,6)]
    # a.gx.T1D <- a.px1 <- a.px2.T1D <- a.px2.AAB <- NULL
    genes <- NULL
    
    # Handle requests from GO/Reactome drop-down selection
    
    if(length(input$GOReactq)) {
      GorR <- isolate(input$GorR)
      if(GorR == "Gene Ontology") {
          genes <- go2genes(isolate(input$GOReactq))
      } else {
        path <- unlist(PATHS[(isolate(input$GOReactq))])
          genes <- path2genes(path)
      }
    }
    
    # Handle requests from entered gene list
    genesIn <- isolate(input$Glist)  
    if(input$highlight & length(genesIn)) { 
        genes <- GENES[genesIn]
        # There's a bug in Plotly that prevents using annotations:
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
    }
    
    if(!is.null(genes)) {
        gx[!Entrez %in% genes, c("Color", "Text") := list("Genes/proteins", "")]
        gx[Entrez %in% genes,  c("Color", "Text") := list("Genes/proteins of interest", Gene)]
        px1[!Entrez %in% genes, c("Color", "Text") := list("Genes/proteins", "")]
        px1[Entrez %in% genes,  c("Color", "Text") := list("Genes/proteins of interest", Protein)]
        px2[!Entrez %in% genes, c("Color", "Text") := list("Genes/proteins", "")]
        px2[Entrez %in% genes,  c("Color", "Text") := list("Genes/proteins of interest", Protein)]
        pal <- pal2 <- c("gray", "red")
    } else {
      gx[, Color := Label]
      px1[, Color := Label]
      px2[, Color := Label]
    }
    
    volcanos <- list()
    if("gx.T1D" %in% input$activeVolcano) {
      volcanos$gx.T1D <- plot_ly(data = gx, x = ~Diff.T1DvsHC, y = ~nlogP.T1DvsHC, type="scatter", mode = "markers",
                        hoverinfo = "text", text = ~paste("Gene: ", Gene, "<br>-log(adjusted p): ", nlogP.T1DvsHC, "<br>Difference: ", Diff.T1DvsHC),
                        color = ~Color, colors = pal2, #textfont = list(color = "#000000"), textposition = "top center",
                        showlegend = FALSE) %>% 
                        layout(xaxis = list(title = "Difference (T1D-HC) | Transcriptomics | Pancreas"), yaxis = list(title= "-log(adjusted p)"))
    }
    if("px1" %in% input$activeVolcano) {
      volcanos$px1 <- plot_ly(data = px1, x = ~Difference, y = ~nlogP, type="scatter", mode = "markers",
                     hoverinfo = "text", text = ~paste("Protein: ", Protein, "<br>Gene: ", Gene, "<br>-log(adjusted p): ", nlogP, "<br>Difference: ", Difference),
                     color = ~Color, colors = pal2, #textfont = list(color = "#000000"), textposition = "top center",
                     showlegend = FALSE) %>% 
                     layout(xaxis = list(title = "Difference (T1D-HC) | Proteomics | Exocrine"), yaxis = list(title = "-log(adjusted p)"))
    }
    if("px2.T1D" %in% input$activeVolcano) {
      volcanos$px2.T1D <- plot_ly(data = px2, x = ~Diff.T1DvsHC, y = ~nlogP.T1DvsHC, type="scatter", mode = "markers",
                         hoverinfo = "text", text = ~paste("Protein: ", Protein, "<br>Gene: ", Gene, "<br>-log(adjusted p): ", nlogP.T1DvsHC, "<br>Difference: ", Diff.T1DvsHC),
                         color = ~Color, colors = pal, #textfont = list(color = "#000000"), textposition = "top center",
                         legendgroup = ~Label, showlegend = FALSE) %>% 
                         layout(xaxis = list(title = "Difference (T1D-HC) | Proteomics | Endocrine"), yaxis = list(title = "-log(adjusted p)"))
    }
    if("px2.AAB" %in% input$activeVolcano) {
      volcanos$px2.AAB <- plot_ly(data = px2, x = ~Diff.AABvsHC, y = ~nlogP.AABvsHC, type="scatter", mode = "markers",
                         hoverinfo = "text", text = ~paste("Protein: ", Protein, "<br>Gene: ", Gene, "<br>-log(adjusted p): ", nlogP.AABvsHC, "<br>Difference: ", Diff.AABvsHC),
                         color = ~Color, colors = pal, #textfont = list(color = "#000000"), textposition = "top center", 
                         legendgroup = ~Color) %>% 
                         layout(xaxis = list(title = "Difference (AAB-HC) | Proteomics | Endocrine"), yaxis = list(title = "-log(adjusted p)"))
    }
    if("gx.AAB" %in% input$activeVolcano) { # Option for showing plot of AABvsHC transcriptomics volcano for UN-adjusted p-values
      gx.AAB <- plot_ly(data = gx, x = ~Diff.AABvsHC, y = ~(nlogP.unadj.AABvsHC), type="scatter", mode = "markers",
                        text = ~paste("Gene: ", Gene, "<br>-log(un-adjusted p): ", nlogP.unadj.AABvsHC, "<br>Difference: ", Diff.AABvsHC),
                        color = ~Color, colors = pal2, showlegend = FALSE) %>%
                        layout(xaxis = list(title = "Difference (AAB-HC) | Transcriptomics | Pancreas"), yaxis = list(title= "-log(NON-adjusted p)"))
      subplot(gx.AAB, volcanos, titleX = T, titleY = T) %>%
        layout(plot_bgcolor = "#F7F7F7FF", legend = list(x = 0, y = 1.2, orientation = "h", bgcolor = "#F7F7F7FF"))
    } else if(length(volcanos)) {
      subplot(volcanos, titleX = T)  %>%
        layout(yaxis = list(title = "-log(adjusted P)"), plot_bgcolor = "#F7F7F7FF", legend = list(x = 0, y = 1.1, orientation = "h", bgcolor = "#FFFFFF"))
    } else {
      return(NULL)
    }
  })
  
  output$multi <- renderPlotly({
    plot_ly()
    
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
