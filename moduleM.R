# Help events -------------------------------------------------#

observeEvent(input$guideMatch, {
  session$sendCustomMessage(type = "startGuideM", message = list(steps = toJSON(steps)))
})

observeEvent(input$cohortDataRequirements, {
  showModal(modalDialog(
    title = "Cohort data requirements",
    includeHTML("Help/uploading_data.html"),
    easyClose = TRUE,
    footer = NULL
  ))
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

output$matchCovariatesC <- renderUI({
  C <- c("BMI", "db.duration", "age.onset", "Cpeptide", "HbA1c", "peak.gluc", 
         "GADA.pos", "IA2A.pos", "mIAA.pos", "ZnT8A.pos", "AutoAb.count")
  tags$div(h4("[ clinical ]"), matchUI(C, cohortdata$matchOpts))
})

output$matchCovariatesD <- renderUI({
  D <- c("age", "sex_Female", "sex_Male", "race_Caucasian", "race_AfricanAmerican", "race_Hispanic.Latino", 
         "race_Asian", "race_AmericanIndian", "race_Multiracial")
  tags$div(h4("[ demographic ]"), matchUI(D, cohortdata$matchOpts))
})

output$matchCovariatesX <- renderUI({
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
  tags$div(id = "matchParameters",
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
  req(!is.null(cohortdata$matchOpts))
  tags$div(id = "matchUI",
    fluidRow(
      column(2,
             HTML("<strong>Data fusion/parameter selection</strong><br>"), 
             helpText("All covariates guessed as shared by both datasets are used as match parameters, which does not necessarily represent the user-desired default.
                    Parameters can be adjusted in a drag-and-drop manner, i.e. bring over and connect those that should be used.")
      ),
      column(2,
             HTML("<strong>You are matching on</strong>"),
             verbatimTextOutput("matchOn", placeholder = TRUE),
             actionButton("match", "Match")),
      column(8,
             uiOutput("matchParameters")
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
  removeUI("#matchUI", immediate = T)
  cohortX <- cohortdata$cohortX
  matchOn <- cohortdata$matchOn
  # Data fusion -- create dataset with required structure
  which.donors <- switch(input$matchType,
                         ND = "No diabetes",
                         T1D = c("T1D", "T1D Medalist"),
                         T2D = "T2D",
                         Aab = "Autoab Pos")
  npod.subset <- npodX[donor.type %in% which.donors]
  fused <- cohortFusion(cohortX, npod.subset, matchOn,
                        c(ifelse(input$cohortName == "", "CohortX", input$cohortName), paste0("nPOD-", input$matchType)))
  cohortdata$fused <- copy(fused)
  # Do match
  matchResult <- Match1(fused, matchOn)
  cohortdata$matchResult <- matchResult$result
  cohortdata$matchedSet <- matchResult$matched
})

output$matchTable <- renderTable({
  if(is.null(cohortdata$matchedSet)) return()
  cohortdata$matchedSet
}, striped = T)

output$matchResult <- renderUI({
  if(is.null(cohortdata$matchResult)) return()
  tags$div(class = "matchOutput",
           h4("Results"),
           tabsetPanel(id = "resultsTabs", type = "tabs",
                       tabPanel("Match table", br(),
                                helpText("A sample of the matched cases"), br(),
                                div(style = "overflow-x: auto; width: 90%;", tableOutput("matchTable")), br(), 
                                downloadButton("exportMatch", "Match table"), br(), br(),
                                HTML("*Matched cases can be requested through this <a href='https://npoddatashare.coh.org/'>portal</a>.")
                       ),
                       tabPanel("Match stats summary", br(),
                                ""
                       ),
                       tabPanel("Advanced match exploration", br(),
                                uiOutput("advancedMatchResult")
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

observe({
  input$switchTabs_advanced
  updateTabsetPanel(session, "resultsTabs", selected = "Advanced match exploration")
})

output$advancedMatchResult <- renderUI({
  if(is.null(cohortdata$matchedSet)) return()
  npod.matches <- cohortdata$matchedSet[, as.numeric(gsub("nPOD_", "", match.ID))]
  cutoff <- length(npod.matches)/2
  npod.matches <- cdata[ID %in% npod.matches]
  n <- npod.matches[, lapply(.SD, function(x) table(is.na(x))["FALSE"])]
  n <- melt(n, measure.vars = names(n))
  n <- n[!variable %in% c("ID", "donor.type") & value > cutoff][order(value, decreasing = T)]
  nPOD <- setNames(as.character(n$variable), n[, paste0(variable, " (", value, ")")])
  cohortX <- c("", names(cohortdata$cohortX)[names(cohortdata$cohortX) != "ID"])
  tags$div(id = "advancedMatchResult", class = "matchOutput",
            fluidRow(
             column(5, br(), 
                    helpText("Aside from comparing group data to see how well matching worked using the selected covariates, 
                              examining other available data may be of interest. 
                              This includes characterization data derived from independent experiments
                              that extend beyond basic demographic and clinical measurements
                              and which may not be complete across all nPOD cases."), br(),
                    h5("nPOD"),
                    selectInput("matchAttributeA", "Features (cases)", choices = nPOD),
                    br(),
                    h5("Your cohort"),
                    selectInput("matchAttributeB", "Features", choices = cohortX),
                    checkboxInput("sameScale", "Plot on same scale"),
                    helpText("")
                    ),
             column(1),
             column(5, br(), br(), br(),
                    plotOutput("matchPlot")),
             column(1)
           )
          )
})

output$matchPlot <- renderPlot({
  if(is.null(cohortdata$matchedSet)) return()
  matches <- cohortdata$matchedSet[, as.numeric(gsub("nPOD_", "", match.ID))]
  var1 <- input$matchAttributeA
  var2 <- input$matchAttributeB
  tmp <- cdata[ID %in% matches, c("ID", "donor.type", var1), with = F]
  tmp[, donor.type := paste0("nPOD-", donor.type)]
  if(grepl("grp$|cat$|score$|bin$|count$|pos$", var1)) tmp[[var1]] <- factor(tmp[[var1]])
  p <- ggplot(tmp, aes_string(x = "donor.type", y = var1)) + 
    geom_dotplot(method = "histodot", stackdir = "center", binaxis = "y", color = "#17a2b8", fill = "#17a2b8") + 
    theme_bw()
  if(var2 == "") {
    return(p)
  } else {
    tmp2 <- merge(cohortdata$matchedSet[, .(donor.type, ID)],  cohortdata$cohortX[, c("ID", var2), with = F], by = "ID")
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