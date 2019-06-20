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
                                          div(class = "forceInline", br(),
                                              actionButton(ns("prevSet"), "Prev 40"),
                                              actionButton(ns("nextSet"), "Next 40"),
                                              htmlOutput(ns("index"))
                                          ),
                                          div(class = "forceInline", style = "margin-left: 10px;",
                                              selectizeInput(ns("subset"), HTML("Browse"),
                                                             choices = c("", unique(cdata[["donor.type"]])), selected = NULL,
                                                             options = list(placeholder = "all in database"), width = 200)
                                             ),
                                          div(class = "forceInline", # style = "margin-left: 50px;",
                                              selectizeInput(ns("selectID"), "Or select specific IDs",
                                                             choices = c("", cdata[["ID"]]), selected = NULL, multiple = T,
                                                             options = list(placeholder = "(max 10)",
                                                                            maxItems = 10), width = 220)
                                              ),
                                          div(class = "forceInline", br(),
                                              actionButton(ns("submitID"), "go")
                                          ),
                                          div(class = "forceInline", style = "margin-right: 150px;",
                                              dataUploadUI(ns("IDlist"), label = "Or upload a list of IDs",
                                                           buttonlabel = "Upload", width = 200)
                                              ),
                                          div(class = "forceInline", style = "background-color: WhiteSmoke;",
                                          div(class = "forceInline", style = "margin-left: 50px;",
                                              selectInput(ns("class"), HTML("<strong>Data annotation</strong>"),
                                                          choices = c("Contributor", "CellTissue", "Level", "ThemeTag", "GOTerm"), width = 200)
                                              ),
                                         div(class = "forceInline",
                                             div(class = "forceInline", uiOutput(ns("expandUI")))
                                         ))
                                 ),
                                 fluidRow(absolutePanel(style = "z-index: 10;", draggable = T,
                                                        conditionalPanel("input.expandon != ''", ns = ns,
                                                          div(class = "subgroups-panel", style = "background-color: WhiteSmoke;",
                                                              div(align = "right", actionButton(ns("closepanel"), "", icon = icon("times"))),
                                                              plotlyOutput(ns("expandplot"), height = 750, width = 500))
                                                        )
                                          )),
                                 fluidRow(
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


  visIDs <- reactiveVal(1:40)
  cdataL <- reactiveVal(cdata)
  customIDs <- reactiveVal(NULL)

  uploadedIDs <- callModule(dataUpload, "IDlist", asDT = F, removable = T, infoRmd = system.file("help/ID_list.Rmd", package = "DIVE"))

  observeEvent(uploadedIDs(), {
    if(is.null(uploadedIDs())) customIDs(NULL) else customIDs(uploadedIDs())
  }, ignoreNULL = F)

  observeEvent(input$submitID, {
    customIDs(input$selectID)
  })

  observeEvent(input$subset, {
    if(input$subset == "") cdataL(cdata) else cdataL(cdata[donor.type == input$subset])
    updateSelectizeInput(session, "selectID", selected = "")
    end <- ifelse(nrow(cdataL()) < 40, nrow(cdataL()), 40)
    visIDs(1:end)
  })

  observeEvent(customIDs(), {
    if(is.null(customIDs())) cdataL(cdata) else cdataL(cdata[ID %in% customIDs()])
    end <- ifelse(nrow(cdataL()) < 40, nrow(cdataL()), 40)
    visIDs(1:end)
  }, ignoreNULL = F)

  output$index <- renderPrint({
      helpText(paste0("displaying ", visIDs()[1], "-", visIDs()[length(visIDs())], " of ", nrow(cdataL())))
  })

  observeEvent(input$nextSet, {
    if(last(visIDs()) < nrow(cdataL())) {
      newrange <- visIDs() + 40
      if(last(newrange) > nrow(cdataL())) newrange <- newrange[newrange <= nrow(cdataL())]
      visIDs(newrange)
    }
  })

  observeEvent(input$prevSet, {
    if(visIDs()[1] != 1) visIDs(visIDs() - 40)
  })

  output$expandUI <- renderUI({
    tags$div(
      div(class = "forceInline", selectInput(session$ns("expandon"), HTML("<strong>Expand on</strong>"), choices = c("", unique(withclass()$Data)), selected = ""))
    )
  })

  observeEvent(input$closepanel, {
    updateSelectInput(session, "expandon", selected = "")
  })

  varL <- reactive({
    IDs <- visIDs()
    varL <- cdataL()[IDs, cdataL()[IDs, sapply(.SD, function(x) !all(is.na(x))), with = F] ]

    for(col in removeID(names(varL))) varL[[col]] <- as.integer(!is.na(varL[[col]]))
    varL <- melt(varL, id.var = "ID", variable.name = "VarID", value.name = "Available")
    varL <- merge(varL, metadata[, c("VarID", "Variable", input$class), with = F], by = "VarID")
    setnames(varL, input$class, "Data")
  })

  withclass <- reactive({
    unique(varL()[, .(ID, Available, Data)])
  })

  output$cases <- renderPlotly({
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
