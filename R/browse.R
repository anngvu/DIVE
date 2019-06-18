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
                                 fluidRow(style = "margin-top: 20px; margin-left: 20px;",
                                          # div(class = "forceInline",
                                          #     radioButtons(ns("nview"), "# of cases in view", choices = c(40), inline = T)
                                          # ),

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
                                                          choices = c("Contributors", "CellTissue", "Level", "ThemeTag", "Method", "GOTerm"))
                                              )
                                          ),
                                 fluidRow(shinycssloaders::withSpinner(plotlyOutput(ns("cases"), height = 750), color = "gray"))
                                 ),
                        tabPanel("By all variables",
                                 DT::DTOutput(ns("table"))
                        )
            )
  )
}

browse <- function(input, output, session) {

  customIDs <- callModule(dataUpload, "IDlist", asDT = F, removable = T, infoRmd = system.file("help/ID_list.Rmd", package = "DIVE"))

  visIDs <- reactiveVal(1:40)

  # observeEvent(input$nview, {
  #   updateActionButton(session, "prevSet", label = paste0("Prev", input$nview))
  #   updateActionButton(session, "nextSet", label = paste0("Next", input$nview))
  # })

  observeEvent(input$nextSet, {
    if(visIDs()[40] < nrow(cdata)) visIDs(visIDs() + 40)
  })

  observeEvent(input$prevSet, {
    if(visIDs()[1] != 1) visIDs(visIDs() - 40)
  })

  output$table <- DT::renderDataTable({
    show <- metadata[, .(Source, Contributors, Variable, Description, IndividualLevelData, InApp, DataSource, DataSourceLink)]
    # if(input$filterDT) {
    #   show <- show[IndividualLevelData == "Yes", ]
    # }
    show[InApp == "yes", DataSourceLink := paste0("<a href='",DataSourceLink,"' target='_blank'"," title='",DataSourceLink,"'>Get from original source","</a>")]
    show[InApp != "yes", DataSourceLink := ""]

  }, escape = FALSE, rownames = F, options = list(dom = 'ftp', pageLength = 10))

  output$cases <- renderPlotly({

    IDs <- if(is.null(customIDs())) visIDs() else cdata$ID %in% customIDs()
    varL <- cdata[IDs, cdata[IDs, sapply(.SD, function(x) !all(is.na(x))), with = F] ]
    for(col in removeID(names(varL))) varL[[col]] <- as.integer(!is.na(varL[[col]]))

    varL <- melt(varL, id.var = "ID", variable.name = "VarID", value.name = "Available")
    withclass <- merge(varL, metadata[, c("VarID", input$class), with = F], by = "VarID")
    withclass <- unique(withclass[, c("ID", "Available", input$class), with = F])
    setnames(withclass, input$class, "Data")

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
