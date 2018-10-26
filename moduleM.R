# Help events -------------------------------------------------#

observeEvent(input$guideMatch, {
  session$sendCustomMessage(type = "startGuideM", message = list(steps = toJSON(steps)))
})

# -------------------------------------------------------------

observeEvent(input$cohortDataUpload, {
  cohortX <- fread(input$cohortDataUpload$datapath, header = T)
  # Perform data checks
  
  # If data looks good...
  setNewCohort(cohortX)
})

observe({
  if(input$cohortName == "ExampleCohort2020") setNewCohort(fread("divid.csv", header = T))
})

setNewCohort <- function(cohortX) { # When there's a new dataset in town...
  names(cohortX) <- make.names(names(cohortX))
  cohortdata$cohortX <- cohortX
  removeUI(".matchOutput", multiple = T, immediate = T)
  cohortdata$matchResult <- NULL
  cohortdata$matchOpts <- guessMatch(names(cohortX)) # initial matching parameters
}


output$matchUIHelp <- renderUI({
  req(!is.null(cohortdata$matchOpts))
  tags$div(class = "matchUI", 
           HTML("<strong>Data fusion/parameter selection</strong><br>"), 
           helpText("All covariates guessed as shared by both datasets are used as match parameters, which does not necessarily represent the user-desired default.
                    Parameters can be adjusted in a drag-and-drop manner, i.e. bring over and connect those that should be used."))
})

output$matchCovariatesC <- renderUI({
  req(!is.null(cohortdata$matchOpts))
  C <- c("BMI", "db.duration", "age.onset", "Cpeptide", "HbA1c", "peak.gluc", 
         "GADA.pos", "IA2A.pos", "mIAA.pos", "ZnT8A.pos", "AutoAb.count")
  tags$div(h4("[ clinical ]"), matchUI(C, cohortdata$matchOpts))
})

output$matchCovariatesD <- renderUI({
  req(!is.null(cohortdata$matchOpts))
  D <- c("age", "sex_Female", "sex_Male", "race_Caucasian", "race_AfricanAmerican", "race_Hispanic.Latino", 
         "race_Asian", "race_AmericanIndian", "race_Multiracial")
  tags$div(h4("[ demographic ]"), matchUI(D, cohortdata$matchOpts))
})

output$matchCovariatesX <- renderUI({
  req(!is.null(cohortdata$matchOpts))
  covariates <- names(cohortdata$cohortX)
  used <- cohortdata$matchOpts
  unused <- setdiff(covariates, used)
  unused <- unused[unused != "ID"]
  tags$div(h4("[ don't use ]"), 
           newOrderInput("cvbank", NULL, items = unused, 
                         connect = paste0("cv", seq_along(cohortdata$matchOpts)), 
                         item_class = "btn btn-sm unused covariate", width = 100)
  )
})

output$matchParameters <- renderUI({
  tags$div(id = "matchParameters", class = "matchUI", 
           fluidRow(
               column(4,
                      uiOutput("matchCovariatesX")
               ),
               column(4,
                      uiOutput("matchCovariatesD")
               ),
               column(4,
                      uiOutput("matchCovariatesC")
               ))
  )
})

output$matchUI <- renderUI({
  tags$div(
    fluidRow(
      column(2,
        HTML("<strong>You are matching on:</strong>"),
        verbatimTextOutput("matchOn", placeholder = TRUE),
        actionButton("match", "Match")),
      column(2,
        uiOutput("matchUIHelp")),
      column(8,
             uiOutput(matchParameters)
      ))
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
                        c(paste0("nPOD-", input$matchType), ifelse(input$cohortName == "", "CohortX", input$cohortName)))
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
  tags$div(id = "matchResult", class = "matchOutput",
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

output$advancedMatchResult <- renderUI({
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
  tags$div(id = "matchAttributes", class = "matchOutput", style="padding-right: 30px;",
           h4("Advanced"),
           tabsetPanel(type = "tabs",
                       tabPanel("Other characterization data", br(), 
                                helpText("You might be interested in looking at all available data to know more about your matched cases. 
                                         This includes characterization data derived from various independent experiments
                                         that extend beyond basic demographic and clinical measurements
                                         and which may not be available for all matches. 
                                         The selection below contains attributes that cover at least half of your match subset."),
                                selectInput("matchAttributeA", "Other features (cases)", choices = nPOD),
                                br(),
                                h5('Compare to a measurement in your dataset'),
                                selectInput("matchAttributeB", "Features in your data", choices = cohortX),
                                checkboxInput("sameScale", "Plot on same scale"),
                                helpText("")
                                )
                       )
           )
})

output$matchPlot <- renderPlot({
  if(is.null(cohortdata$matchedSet)) return()
  matched <- cohortdata$matchedSet[grep("nPOD", ID), as.numeric(gsub("nPOD_", "", ID))]
  var1 <- input$matchAttributeA
  var2 <- input$matchAttributeB
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

output$advancedMatchResult2 <- renderUI({
  if(is.null(cohortdata$matchResult)) return()
  tags$div(id = "matchPlot", class = "matchOutput",
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