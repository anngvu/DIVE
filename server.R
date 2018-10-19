library(shiny)

#-- SET-UP ----------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#

shinyServer(function(input, output, session) {
  
  #-- HELP TOURS ----------------------------------------------------------------------------------#
  
  observeEvent(input$helpCohortIn, {
    session$sendCustomMessage(type = "setHelpContent", message = list(steps = toJSON(steps)))
    session$sendCustomMessage(type = "startHelp", message = list(""))
  })
  
  observeEvent(input$helpCorrelation, {
    session$sendCustomMessage(type = "setHelpContent", message = list(steps = toJSON(steps2)))
    session$sendCustomMessage(type = "startHelp", message = list(""))
  })
  
  
  #-- HOME ----------------------------------------------------------------------------------------#
  
  output$network <- renderVisNetwork({
    g %>% visIgraphLayout(randomSeed = 88) %>% visOptions(highlightNearest = TRUE)
  })
  
  observeEvent(input$accessdata, {
    updateNavbarPage(session, "main", selected = "source-data")
  })
  
  # -------------------------------------------------------------
  
  #-- PAGE 1 ----------------------------------------------------------------------------------------#
  
  observeEvent(input$cohortDataUpload, {
    # Remove previous match results
    removeUI(".matchOutput", multiple = T, immediate = T)
    cohortX <- fread(input$cohortDataUpload$datapath, header = T)
    # TO DO: perform data checks
    names(cohortX) <- make.names(names(cohortX))
    cohortdata$cohortX <- cohortX
    cohortdata$matchResult <- NULL
    # Set initial default matching parameters
    cohortdata$matchOpts <- guessMatch(names(cohortX))
  })
  
  output$matchUIhelp <- renderUI({
    req(!is.null(cohortdata$matchOpts))
    tags$div(class = "matchUI", 
            HTML("<strong>Data fusion/parameter selection</strong><br>"), 
            helpText("All covariates guessed as shared by both datasets are used as match parameters, which does not necessarily represent the user-desired default.
                      Parameters can be adjusted in a drag-and-drop manner, i.e. bring over and connect those that should be used."))
  })
  
  output$matchCovariatesC <- renderUI({
    cohortdata$cohortX
    req(!is.null(cohortdata$matchOpts))
    C <- c("BMI", "db.duration", "age.onset", "Cpeptide", "HbA1c", "peak.gluc", 
           "GADA.pos", "IA2A.pos", "mIAA.pos", "ZnT8A.pos", "AutoAb.count")
    tags$div(class = "matchUI", h4("[ clinical ]"), matchUI(C, cohortdata$matchOpts))
  })
  
  output$matchCovariatesD <- renderUI({
    cohortdata$cohortX
    req(!is.null(cohortdata$matchOpts))
    D <- c("age", "sex_Female", "sex_Male", "race_Caucasian", "race_AfricanAmerican", "race_Hispanic.Latino", 
           "race_Asian", "race_AmericanIndian", "race_Multiracial")
    tags$div(class = "matchUI", h4("[ demographic ]"), matchUI(D, cohortdata$matchOpts))
  })
  
  output$matchCovariatesX <- renderUI({
    req(!is.null(cohortdata$matchOpts))
    covariates <- names(cohortdata$cohortX)
    used <- cohortdata$matchOpts
    unused <- setdiff(covariates, used)
    unused <- unused[unused != "ID"]
    tags$div(class = "matchUI", h4("[ don't use ]"), 
            newOrderInput("cvbank", NULL, items = unused, 
                  connect = paste0("cv", seq_along(cohortdata$matchOpts)), 
                  item_class = "btn btn-sm unused covariate", width = 100)
    )
  })
  
  observe({
     cvs <- paste0("cv", seq_along(cohortdata$matchOpts), "_order")
     names(cvs) <- names(cohortdata$matchOpts)
     cohortdata$matchOn <- unlist(lapply(cvs, function(a) input[[a]]))
  })
  
  output$matchOn <- renderPrint({
    cat(cohortdata$matchOn, sep="\n")
  })
  
  observeEvent(input$match, {
    removeUI(".matchUI", multiple = T, immediate = T)
    removeUI(".matchOutput", multiple = T, immediate = T)
    cohortX <- cohortdata$cohortX
    matchOn <- cohortdata$matchOn
    # Data fusion -- create dataset with required structure
    which.donors <- switch(input$matchType,
                           ND = "No diabetes",
                           T1D = c("T1D", "T1D Medalist"),
                           T2D = "T2D",
                           Aab = "Autoab Pos")
    npod.subset <- npodX[donor.type %in% which.donors]
    fused <- cohortFusion(npod.subset, cohortX, matchOn,
                          c(paste0("nPOD-", input$matchType), ifelse(input$cohortname == "", "CohortX", input$cohortname)))
    cohortdata$fused <- copy(fused)
    # Do match
    matchResult <- Match2(fused, matchOn)
    cohortdata$matchResult <- matchResult$result
    cohortdata$matchedSet <- matchResult$matched
  })
  
  output$matchSummary <- renderTable({
    if(is.null(cohortdata$matchedSet)) return()
    cohortdata$matchedSet
  })
  
  output$matchResult <- renderUI({
    if(is.null(cohortdata$matchResult)) return()
    tags$div(class = "matchOutput",
             h4("Results"),
             tabsetPanel(type = "tabs",
               tabPanel("Match preview", br(),
                 tableOutput("matchSummary"), 
                 downloadButton("exportMatch", "Match table"), br(), br(),
                 HTML("*Samples of matched cases can be requested through this <a href='https://npoddatashare.coh.org/'>portal</a>.") 
               ),
               tabPanel("Match stats summary",
                        br(),
                        downloadButton("downloadSummary", "Match stats summary")       
               )
             )
    )
  })
  
  output$exportMatch <- downloadHandler(
    filename = function() {
      "match_result.csv"
    },
    content = function(file) {
      write.csv(cohortdata$fused, file, row.names = F)
    }
  )
  
  output$exploreMatchData <- renderUI({
    if(is.null(cohortdata$matchedSet)) return()
    matched <- cohortdata$matchedSet[grep("nPOD", ID), as.numeric(gsub("nPOD_", "", ID))]
    cutoff <- length(matched)/2
    matched <- cdata[ID %in% matched]
    n <- matched[, lapply(.SD, function(x) table(is.na(x))["FALSE"])]
    n <- melt(n, measure.vars = names(n))
    n <- n[!variable %in% c("ID", "donor.type") & value > cutoff][order(value, decreasing = T)]
    nPOD <- setNames(as.character(n$variable), n[, paste0(variable, " (", value, ")")])
    cohortX <- names(cohortdata$cohortX)
    cohortX <- c("", cohortX[cohortX != "ID"])
    tags$div(class = "matchOutput", style="padding-right: 30px;",
             h4("Advanced"),
             tabsetPanel(type = "tabs",
                         tabPanel("Other characterization data", br(), 
               helpText("You might be interested in looking at all available data to know more about your matched cases. 
                        This includes characterization data derived from various independent experiments
                        that extend beyond basic demographic and clinical measurements
                        and which may not be available for all matches. 
                        The selection below contains attributes that cover at least half of your match subset."),
               selectInput("matchAttribute", "Other features (cases)", choices = nPOD),
               br(),
               h5('Compare to a measurement in your dataset'),
               selectInput("matchAttribute2", "Features in your data", choices = cohortX),
               checkboxInput("sameScale", "Plot on same scale"),
               helpText("")
                         )
             )
    )
  })
  
  output$matchPlot <- renderPlot({
    if(is.null(cohortdata$matchedSet)) return()
    matched <- cohortdata$matchedSet[grep("nPOD", ID), as.numeric(gsub("nPOD_", "", ID))]
    var1 <- input$matchAttribute
    var2 <- input$matchAttribute2
    tmp <- cdata[ID %in% matched, c("ID", "donor.type", var1), with = F]
    tmp[, donor.type := paste0("nPOD-", donor.type)]
    p <- ggplot(tmp, aes_string(x = "donor.type", y = var1)) + 
      geom_dotplot(method = "histodot", stackdir = "center", binaxis = "y", color = "#17a2b8", fill = "#17a2b8") + 
      theme_bw()
    if(var2 == "") {
      return(p)
    } else {
      ids <- cohortdata$matchedSet[grep("nPOD", ID, invert = T), ID]  
      tmp2 <- cohortdata$cohortX[ID %in% ids, c("ID", var2), with = F]
      tmp2[, donor.type := "CohortX"]
      if(input$sameScale) {
        setnames(tmp2, old = var2, new = var1)
        tmp2 <- rbind(tmp, tmp2, use.names = T)
        colors <- setNames(c("#17a2b8", "indianred"), unique(tmp2$donor.type))
        p <- p + geom_dotplot(data = tmp2, 
                              aes_string(x = "donor.type", y = var1, color = "donor.type", fill = "donor.type"), 
                              method = "histodot", stackdir = "center", binaxis = "y") +
          scale_color_manual(values = colors) + scale_fill_manual(values = colors)
        return(p)
      } 
      q <- ggplot(tmp2, aes_string(x = "donor.type", y = var2)) + 
        geom_dotplot(method = "histodot", stackdir = "center", binaxis = "y",
                     color = "indianred", fill = "indianred") + 
        theme_bw()
      gridExtra::grid.arrange(grobs = list(p, q), ncols = 2, nrow = 1)
    }
  })
  
  output$exploreMatchData2 <- renderUI({
    if(is.null(cohortdata$matchResult)) return()
    tags$div(class = "matchOutput",
             h4("Viewer"), br(),
             plotOutput("matchPlot")
    )
  })
  
  output$npodgraph <- renderPlotly({
    npodgraph
  })
  
  output$nPie <- renderPlotly({
    hover <- event_data("plotly_hover")
    if(is.null(hover$key)) return()
    if(hover$key == "NA") {
      pie <- ndata[variable == hover$x & value == hover$y, table(donor.type)]
    } else {
      pie <- ndata[variable == hover$key & value == hover$y, table(donor.type)]
    }
    pie <- melt(pie, measure.vars = names(pie))
    pie <- pie[order(pie$value), ]
    colors <- as.character(pie$donor.type[order(pie$value)])
    colors <- c(ppColors, "Pending" = "gray", "Pregnancy" = "pink", "Transplant" = "darkseagreen4")[colors]
    colors <- apply(col2rgb(colors), 2, function(x) paste0("rgb(", x[1], ",", x[2],",", x[3], ")"))
    p <- plot_ly(pie, labels = ~donor.type, values = ~value, type = 'pie', sort = F,
                 textinfo = "label", hole = 0.3, showlegend = F,
                 marker = list(colors = colors),
                 width = 250, height = 300,
                 insidetextfont = list(color = "#FFFFFF")) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor= "transparent", plot_bgcolor = "transparent",
             autosize = F, margin = list(t = 100, b = 100, r = 25, l = 80),
             font = list(size = 10))
    p
  })
  
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
  
  # Menu events -------------------------------------------------
  
  observe({
    opt <- input$varMenuOpt
    if(opt == "variable") {
      updateSelectizeInput(session, "varMenu", "Exclude/keep in correlation matrix:", colnames(plotdata$corr$corM))
    } else if(opt == "category") {
      
    } else {
      AuthYr <- unique(gsub("_.*$", "", colnames(plotdata$corr$corM)))
      AuthYr <- sapply(AuthYr, function(s) paste0(substr(s, 1, nchar(s)-2), " et al. 20", substr(s, nchar(s)-1, nchar(s))))
      AuthYr <- setNames(names(AuthYr), AuthYr)
      updateSelectizeInput(session, "varMenu", "Exclude/keep in correlation matrix:", AuthYr)
    }
  })
  
  # observeEvent(input$varExclude, {
  #   corr <- plotdata$corr.last.state
  #   vars <- input$varMenu
  #   corr <- corr[grep(), !colnames(corr) %in% vars]
  #   plotdata$corr <- corr
  # })
  # 
  # observeEvent(input$varKeep, {
  #   corr <- plotdata$corr.last.state
  #   vars <- input$varMenu
  #   corr <- corr[rownames(corr) %in% vars, colnames(corr) %in% vars]
  #   plotdata$corr <- corr
  # })
  
  observeEvent(input$varReset, {
    plotdata$corr <- plotdata$corr.last.state
    updateSelectizeInput(session, "varMenu", "Exclude/keep in correlation matrix:", selected = character(0))
  })

  # Plot events ----------------------------------------------------------
  #  Correlations
  output$corM <- renderPlotly({
    corr <- plotdata$corr$corM
    corrN <- plotdata$corr$corN
    corr[corrN < input$minimumN] <- NA  # Gray out/remove cor values calculated from less than specified n
    has.n <- apply(corrN, 1, max) >= input$minimumN
    corr <- corr[has.n, has.n]
    p <- plot_ly(x = rownames(corr), y = colnames(corr), z = corr, type = "heatmap", source = "correlation", colorscale = "RdBu",
                 width = 1200, height = 1000, colorbar = list(thickness = 10)) %>%
      layout(xaxis = list(title = "", showgrid = F, showticklabels = FALSE, ticks = ""), 
             yaxis = list(title = "", showgrid = F, showticklabels = FALSE, ticks = ""), 
             plot_bgcolor = "gray")
    p
  })
  
  observeEvent(input$dataUpload, {
    uploaded.data <- fread(input$dataUpload$datapath, header = T)
    # TO DO: Perform some checks of file
    
    names(uploaded.data) <- make.names(names(uploaded.data))
    plotdata$uploaded.data <- uploaded.data
    newdata <- merge(cdata, uploaded.data, by.x = "ID", by.y = names(uploaded.data)[1], all = T)
    plotdata$cdata <- newdata
    newcorrs <- data2cor(newdata)
    plotdata$corr <- plotdata$corr.last.state <- newcorrs
    newchoices <- rownames(newcorrs$corM)
    updateSelectizeInput(session, "drilldown", "Drill down to data for", choices = c("", newchoices),
                         selected = "", options = list(maxItems = 2))
    updateSelectizeInput(session, "varMenu", "Exclude/keep in correlation matrix:", choices = newchoices)
    # updateSelectInput(session, "colorby", "Color data points by", choices = newchoices, selected = "donor.type")
  })
  
  output$scatter <- renderPlotly({
    req(!is.null(input$drilldown))
    drilldown <- input$drilldown
    tmp <- as.data.frame(plotdata$cdata)
    var1 <- drilldown[1]
    var2 <- drilldown[2]
    if(grepl("grp$|cat$|score$|bin$|count$", var1)) tmp[[var1]] <- factor(tmp[[var1]])
    if(!is.na(var2)) { # -> scatter plot 2-variable view
      tmp <- tmp[complete.cases(tmp[, c(var1, var2)]), ]
      if(grepl("grp$|cat$|score$|bin$|count$", var2)) tmp[[var2]] <- factor(tmp[[var2]])
      p <- ggplot(tmp, aes_string(x = var1, y = var2)) + 
        labs(title = paste("n =", nrow(tmp))) +
        theme_bw()
      if(all(grepl("grp$|cat$|score$|bin$", c(var1, var2)))) {
        p <- p + geom_count() # when both variables are categorical, this deals with overplotting
      } else {
        p <- p + geom_point(aes_string(color = input$colorby), size = 2, alpha = 0.7)
      }
      if(input$colorby == "donor.type") { # default is to color points by donor.type
        p <- p + scale_colour_manual(values = ppColors)
      } else if(grepl("grp$|cat$", input$colorby)) { # allow to color by limited set of categorical vars?
        # not yet implemented
        
      } else { # continuous color scale for interval variable
        p <- p + scale_colour_distiller(palette = "YlOrRd", na.value = "black")
      }
      if(input$plotsmooth) p <- p + stat_smooth(method = "lm")
      if(input$switchXY) p <- p + coord_flip()
      p <- ggplotly(p)
      p
    } else { # boxplot 1-variable view
      tmp <- tmp[!is.na(tmp[[var1]]), ]
      tmp$donor.type <- factor(tmp$donor.type)
      p <- ggplot(tmp, aes_string(x = "donor.type", y = var1)) +
        geom_boxplot(outlier.color = NA) + 
        scale_colour_manual(values = ppColors) +
        labs(title = paste("n =", nrow(tmp))) +
        theme_bw()
      if(is.factor(tmp[[var1]])) {
        p <- p + geom_count(aes(color = donor.type)) 
      } else {
        p <- p + geom_point(aes(color = donor.type), size = 2, alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05))
      }
      p <- ggplotly(p)
      p$x$data[[1]]$marker$opacity <- 0 # Manual specification since plotly doesn't translate ggplot settings for boxplot
      p <- hide_legend(p)
      if(length(levels(tmp$donor.type)) > 4) p <- p %>% layout(xaxis = list(tickangle = 45))
      p
    } 
  })
  
  observe({
    s <- event_data("plotly_click", source = "correlation")
    if(length(s)) {
      var1 <- s[["x"]]
      var2 <- s[["y"]]
      updateSelectizeInput(session, "drilldown", "Drill down to data for", selected = c(var1, var2))
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
  
  output$downloadCollection <- downloadHandler(
    filename = function() {
      "Archive.zip"
    },
    content = function(file) {
      file.copy("Collection/Archive.zip", file)
    },
    contentType = "application/zip"
  )
  
})
