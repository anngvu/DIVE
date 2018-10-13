library(shiny)

#-- SET-UP ----------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------------#

shinyServer(function(input, output, session) {
  
  #-- HOME ----------------------------------------------------------------------------------------#
  
  output$network <- renderVisNetwork({
    g %>% visIgraphLayout(randomSeed = 88) %>% visOptions(highlightNearest = TRUE)
  })
  
  observeEvent(input$accessdata, {
    updateNavbarPage(session, "main", selected = "source-data")
  })
  
  # Help events -------------------------------------------------
  # Intro.js demo
  
  # -------------------------------------------------------------
  
  #-- PAGE 1 ----------------------------------------------------------------------------------------#
  
  observeEvent(input$cohortDataUpload, {
    cohortX <- fread(input$cohortDataUpload$datapath, header = T)
    # TO DO: perform data checks
    
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
                      Parameters can be adjusted in a drag-and-drop manner, i.e. relocate them to the relevant slot."))
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
    removeUI(".matchUI *", multiple = T, immediate = T)
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
             h4("Result preview"),
             tableOutput("matchSummary"), 
             downloadButton("downloadSummary", "Match stats summary"), downloadButton("exportMatch", "Match table"), br(), br(),
             HTML("*Samples from matched cases can be requested through this <a href='https://npoddatashare.coh.org/'>portal</a>."))
  })

  output$npodgraph <- renderPlotly({
    npodgraph
  })
  
  output$exportMatch <- downloadHandler(
    filename = function() {
      "match_result.csv"
    },
    content = function(file) {
      write.csv(cohortdata$fused, file, row.names = F)
    }
  )
  
  output$otherAttributes <- renderTable({
    if(is.null(cohortdata$matchedSet)) return()
    matched <- cohortdata$matchedSet[grep("nPOD", ID), as.numeric(gsub("nPOD_", "", ID))]
    cutoff <- length(matched)/2
    matched <- cdata[ID %in% matched]
    n <- matched[, lapply(.SD, function(x) table(is.na(x))["FALSE"])]
    n <- melt(n, measure.vars = names(n), variable.name = "Variable", value.name = "N")
    n <- n[!Variable %in% c("ID", "donor.type") & N > cutoff][order(N, decreasing = T)]
    as.data.frame(n)
  }, width = 400)
  
  output$exploreMatchData <- renderUI({
    if(is.null(cohortdata$matchResult)) return()
    tags$div(class = "matchOutput", style="background-color: honeydew",
             h4("Other measurement data available for matched nPOD cases"), br(),
             helpText("Showing all data that are available for at least half of matched nPOD cases."),
             tableOutput("otherAttributes")
    )
  })
  
  output$exploreMatchData2 <- renderUI({
    if(is.null(cohortdata$matchResult)) return()
    tags$div(class = "matchOutput", style="background-color: azure",
             h4("More data views"), br()
    )
  })
  
  # observe({
  #   s <- event_data("plotly_click", source = "npodgraph")
  #   if(length(s)) {
  #     Var <- s[["x"]]
  #     Value <- s[["y"]]
  #     updateSelectizeInput(session, "drilldown", "Drill down to data points for V1, or V1 x V2:", selected = c(Var1, Var2))
  #   } else {
  #     return(NULL)
  #   }
  # })
  
  
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
      title = "Exploring correlations with all data",
      HTML(""),
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
                 width = 1200, height = 1000) %>%
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
    updateSelectizeInput(session, "drilldown", "Drill down to data points for V1, or V1 x V2:", choices = c("", newchoices),
                         selected = "", options = list(maxItems = 2))
    updateSelectizeInput(session, "varMenu", "Exclude/keep in correlation matrix:", choices = newchoices)
    # updateSelectInput(session, "colorby", "Color data points by", choices = newchoices, selected = "donor.type")
  })
  
  output$scatter <- renderPlotly({
    req(!is.null(input$drilldown))
    drilldown <- input$drilldown
    tmp <- as.data.frame(plotdata$cdata)
    Var1 <- drilldown[1]
    if(length(drilldown) == 2) { # Scatter plot for 2-variable view
      Var2 <- drilldown[2]
      tmp <- tmp[complete.cases(tmp[, c(Var1, Var2)]), ]
      if(grepl("grp$|cat$|score$|bin$", Var1)) tmp[[Var1]] <- factor(tmp[[Var1]]) 
      if(grepl("grp$|cat$|score$|bin$", Var2)) tmp[[Var2]] <- factor(tmp[[Var2]])
      p <- ggplot(tmp, aes_string(x = Var1, y = Var2)) + 
        geom_point(aes_string(color = input$colorby), size = 2) + 
        labs(title = paste("n =", nrow(tmp))) +
        theme_bw()
      if(input$colorby == "donor.type") {
        p <- p + scale_colour_manual(values = ppColors)
      } else if(grepl("grp$|cat$", input$colorby)) { # if is a categorical variable
        # p <- p + scale_color_d3("category20")
      } else { # interval variable
        p <- p + scale_colour_distiller(palette = "YlOrRd", na.value = "black")
      }
      # if(all(grepl("grp$|cat$|score$|bin$", c(Var1, Var2)))) p <- p %>% facet_wrap() TO DO: better plots for cat x cat vars
      if(input$plotsmooth) p <- p + stat_smooth(method = "lm")
      if(input$switchXY) p <- p + coord_flip()
      p <- ggplotly(p)
      p
    } else { # Boxplot for 1-variable view
      tmp <- tmp[!is.na(tmp[[Var1]]), ]
      tmp$donor.type <- factor(tmp$donor.type)
      p <- ggplot(tmp, aes_string(x = "donor.type", y = Var1)) +
        geom_boxplot(outlier.color = NA) + 
        geom_point(aes(color = donor.type), size = 2, position = position_jitter(width = 0.05, height = 0.05)) +
        scale_colour_manual(values = ppColors) +
        labs(title = paste("n =", nrow(tmp))) +
        theme_bw()
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
