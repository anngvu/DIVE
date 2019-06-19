#' Shiny app UI for browsing metadata
#'
#' Shiny app UI for browsing metadata
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @export
browseUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
            if(!is.null(CSS)) includeCSS(CSS),
            tabsetPanel(id = ns("tabs"),
                        tabPanel("By nPOD case",
                                 fluidRow(style = "margin-top: 20px; margin-left: 20px;", height = 100,
                                          div(class = "forceInline", style = "margin-left: 10px;",
                                              br(),
                                              actionButton(ns("prevSet"), "Prev 40"),
                                              actionButton(ns("nextSet"), "Next 40")
                                              ),
                                          div(class = "forceInline", style = "margin-left: 100px;",
                                              dataUploadUI(ns("IDlist"), label = "<strong>View by custom list of IDs</strong>", buttonlabel = "Upload")
                                              ),
                                          div(class = "forceInline", style = "margin-left: 100px;",
                                              selectInput(ns("class"), "Data annotation",
                                                          choices = c("Contributor", "CellTissue", "Level", "ThemeTag", "GOTerm"))
                                              ),
                                         div(class = "forceInline", style = "margin-left: 100px;",
                                             div(class = "forceInline", uiOutput(ns("expandUI")))
                                         )
                                 ),
                                 fluidRow(absolutePanel(style = "z-index: 10;", draggable = T,
                                                        conditionalPanel("input.expandon != ''", ns = ns,
                                                          div(class = "subgroups-panel", style = "background-color: ghostwhite;",
                                                              div(align = "right", actionButton(ns("closepanel"), "", icon = icon("times"))),
                                                              plotlyOutput(ns("expandplot"), height = 750, width = 500))
                                                        )
                                          )),
                                 fluidRow(flex = c(1, NA),
                                   shinycssloaders::withSpinner(plotlyOutput(ns("cases"), height = 750), color = "gray")
                            )),
                        tabPanel("By all data",
                                 helpText("'Get from original source' link points to the original data in a supplemental file
                    or to an external database where data has been deposited. The original sources can provide more detail about
                    methodology, definitions and other metadata, but are in a variety of formats not universally machine-readable (e.g. PDF, Excel).
                    To facililate re-use, curated data can also be downloaded all at once (except for some high-throughput datasets)
                    as a collection of plain text tab-delimited  files."),
                                 DT::DTOutput(ns("table")),
                                 downloadButton(ns("download"), label = "Download Collection")
                        )
            )
  )
}

browse <- function(input, output, session) {

  customIDs <- callModule(dataUpload, "IDlist", asDT = F, removable = T, infoRmd = system.file("help/ID_list.Rmd", package = "DIVE"))

  visIDs <- reactiveVal(1:40)

  observeEvent(input$nextSet, {
    if(visIDs()[40] < nrow(cdata)) visIDs(visIDs() + 40)
  })

  observeEvent(input$prevSet, {
    if(visIDs()[1] != 1) visIDs(visIDs() - 40)
  })

  output$expandUI <- renderUI({
    tags$div(
      div(class = "forceInline", selectInput(session$ns("expandon"), "Expand on", choices = c("", unique(withclass()$Data)), selected = ""))
    )
  })

  observeEvent(input$closepanel, {
    updateSelectInput(session, "expandon", selected = "")
  })

  varL <- reactive({
    IDs <- if(is.null(customIDs())) visIDs() else cdata$ID %in% customIDs()
    varL <- cdata[IDs, cdata[IDs, sapply(.SD, function(x) !all(is.na(x))), with = F] ]
    for(col in removeID(names(varL))) varL[[col]] <- as.integer(!is.na(varL[[col]]))
    varL <- melt(varL, id.var = "ID", variable.name = "VarID", value.name = "Available")
    varL <- merge(varL, metadata[, c("VarID", "Variable", input$class), with = F], by = "VarID")
    setnames(varL, input$class, "Data")
  })

  withclass <- reactive({
    unique(varL()[, .(ID, Available, Data)])
  })

  output$cases <- renderPlotly({
    input$class
    withclass <- withclass()
    p <- plot_ly(withclass, x = ~Data, y = ~ID, name = "Available", type = "scatter", mode = "markers",
                 marker = list(size = ~Available *6, color = "#404040", opacity = 1, line = list(color = "#404040")),
                 showlegend = FALSE) %>%
      layout(xaxis = list(type = "category", title = "Data", showline = FALSE, showgrid = FALSE),
             yaxis = list(type = "category", title = "Case", showline = FALSE, dtick = 1, tickfont = list(size = 9), showgrid = FALSE))

    sumID <- withclass[, sum(Available), by = "ID"]
    marginID <- plot_ly(
      x = sumID$V1,
      y = sumID$ID,
      name = "Data Count",
      type = "bar",
      marker = list(color = "#2196f3"),
      showlegend = FALSE
    )  %>% config(displayModeBar = F)

    sumVar <- withclass[, sum(Available), by = "Data"]
    marginVar <- plot_ly(
      x =  sumVar$Data,
      y = sumVar$V1,
      name = "Case Count",
      type = "bar",
      marker = list(color = "DeepPink"),
      showlegend = FALSE
    )

    subplot(marginVar, plotly_empty(), p, marginID,
            nrows = 2,
            shareX = T, shareY = T,
            widths = c(0.9, 0.1), heights = c(0.2, 0.8))

  })

  output$expandplot <- renderPlotly({
    if(!length(input$expandon)) return()
    xvars <- varL()[Data %in% input$expandon, .(Variable, ID, Available)]
    plot_ly(xvars, x = ~Variable, y = ~ID, type = "scatter", mode = "markers",
            marker = list(size = ~Available *6, color = "#404040", opacity = 1, line = list(color = "#404040")),
            showlegend = FALSE) %>%
      layout(xaxis = list(type = "category", title = "Data", showline = FALSE, showgrid = FALSE),
             yaxis = list(type = "category", title = "Case", showline = FALSE, dtick = 1, tickfont = list(size = 9), showgrid = FALSE),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent") %>%
      config(displayModeBar = F)
  })

  output$table <- DT::renderDataTable({
    show <- metadata[, .(Source, Contributor, Variable, Description, OBITerm, IndividualLevelData, InApp, DataSource, DataSourceLink)]
    # if(input$filterDT) {
    #   show <- show[IndividualLevelData == "Yes", ]
    # }
    show[InApp == "yes", DataSourceLink := paste0("<a href='",DataSourceLink,"' target='_blank'"," title='",DataSourceLink,"'>Get from original source","</a>")]
    show[InApp != "yes", DataSourceLink := ""]

  }, escape = FALSE, rownames = F, options = list(dom = 'ftp', pageLength = 10))

  # output$downloadCollection <- downloadHandler(
  #   filename = function() {
  #     "Archive.zip"
  #   },
  #   content = function(file) {
  #     file.copy("Collection/Archive.zip", file)
  #   },
  #   contentType = "application/zip"
  # )

}

#' Launch Shiny app for browsing metadata
#'
#' Wrapper to launch app at console
#'
#' @export
browseR <- function() {
  ui <- browseUI("metadata")
  server <- function(input, output, session) { callModule(browse, "metadata") }
  shinyApp(ui = ui, server = server)
}
