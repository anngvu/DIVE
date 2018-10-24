# Help events -------------------------------------------------#

observeEvent(input$guideCorrelation, {
  session$sendCustomMessage(type = "startGuideC", message = list(steps = toJSON(steps2)))
})

observeEvent(input$helpUpload, {
  showModal(modalDialog(
    title = "Uploading data",
    includeHTML("uploading_data.html"),
    easyClose = TRUE,
    footer = NULL
  ))
})

# Menu events -------------------------------------------------#

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
               height = 1000, colorbar = list(thickness = 8)) %>%
    layout(xaxis = list(title = "", showgrid = F, showticklabels = FALSE, ticks = "", linecolor = "gray", mirror = T), 
           yaxis = list(title = "", showgrid = F, showticklabels = FALSE, ticks = "", linecolor = "gray", mirror = T), 
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
