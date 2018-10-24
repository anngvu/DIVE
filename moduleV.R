# Help events -------------------------------------------------

observeEvent(input$guideVolcano, {
  
})

# -------------------------------------------------------------

# Update genes of interest drop-down
updateSelectizeInput(session, "Glist", "Genes (proteins) of interest", choices = names(GENES), 
                     selected = character(0), options = list(maxItems = 50), server = T)

# Update GO/Reactome drop-down
observe({
  branch <- switch(input$BPCCMP,
                   BP = setNames(GO.BP$GO, paste0(GO.BP$GOTerm, " (", GO.BP$Count, ")")),
                   MF = setNames(GO.MF$GO, paste0(GO.MF$GOTerm, " (", GO.MF$Count, ")")),
                   CC = setNames(GO.CC$GO, paste0(GO.CC$GOTerm, " (", GO.CC$Count, ")"))
  )
  if(input$GorR == "Gene Ontology") {
    updateSelectizeInput(session, "GOReactq", "GO term", choices = branch, selected = character(0), server = TRUE)
  } else {
    updateSelectizeInput(session, "GOReactq", "Reactome pathway", choices = names(PATHS), selected = character(0), server = TRUE)
  }
})

observeEvent(input$gSets, {
  showModal(modalDialog(
    HTML("<strong>Quick lists</strong><br><li>"),
    actionLink("T1Dbase", "T1Dbase genes"),
    helpText("To suggest additional lists, email <>"),
    HTML("<br><br><strong>Custom list</strong><br>"),
    helpText("Text file should have one gene per line. (50 max)"),
    fileInput("listUpload", "", multiple = FALSE, width = "300px", 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
              buttonLabel = "My list"),
    easyClose = TRUE,
    footer = NULL
  ))
})

observeEvent(input$T1Dbase, {
  updateSelectizeInput(session, "Glist", "Genes (proteins) of interest", choices = names(GENES), 
                       selected = readLines("Data/t1dbase.txt"), options = list(maxItems = 50), server = T)
})

observeEvent(input$Ont, {
  genesOn <- isolate(input$GOReactq)
  if(length(genesOn)) {
    GorR <- isolate(input$GorR)
    if(GorR == "Gene Ontology") {
      plotdata$genes <- go2genes(genesOn)
    } else {
      path <- unlist(PATHS[(isolate(input$GOReactq))])
      plotdata$genes <- path2genes(path)
    }
  }
})

observeEvent(input$highlight, {
  genesIn <- isolate(input$Glist)
  if(length(genesIn)) { #
    plotdata$genes <- GENES[genesIn]
  }
})

observeEvent(input$resetVolcano, {
  plotdata$genes <- NULL
})

# Volcano render
output$volcanos <- renderPlotly({
  pal2 <- pal[c(1,3,6)]
  genes <- plotdata$genes
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
                                showlegend = FALSE) %>% 
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

observe({
  input$clistAdd
  cvars <- isolate(input$Clist)
  genes <- isolate(plotdata$genes)
  if(!length(genes) | !length(cvars)) return(NULL)
  xdata <- switch(input$whichX,
                  gx = gxGet(genes),
                  px1 = px1Get(genes),
                  px2 = px2Get(genes)
  )
  genes <- names(xdata)[!names(xdata) %in% c("ID", "ID2")] # keep track of genes present in returned dataset
  xdata <- xdataMerge(xdata, cvars)
  plotdata$xdata <- list(data = xdata, genes = genes, cvars = cvars)
})

output$parallel <- renderPlotly({
  req(!is.null(plotdata$xdata))
  xdata <- plotdata$xdata$data
  genes <- plotdata$xdata$genes
  cvars <- plotdata$xdata$cvars
  p <- plot_ly(data = xdata)
  for(g in genes) {
    p <- p %>% add_trace(name = gsub(".", "-", g, fixed = T), x = as.formula(paste0("~", g)), y = as.formula(paste0("~", cvars)),
                         type = "scatter", mode = "markers", text = ~donor.type, 
                         # symbol = ~donor.type, symbols = c("circle", "triangle-up", "square"), // plotly needs to resolve combining multiple aesthetics in legend
                         marker = list(size = 10))
  }
  p
})