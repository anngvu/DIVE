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
                        tabPanel("By donor",
                                 fluidRow(style = "margin-top: 20px; margin-left: 20px;", height = 100,
                                          div(class = "forceInline", br(),
                                              actionButton(ns("prevSet"), "Prev 40"),
                                              actionButton(ns("nextSet"), "Next 40"),
                                              htmlOutput(ns("index"))
                                          ),
                                          div(class = "forceInline", style = "margin-left: 10px;",
                                              selectizeInput(ns("subset"), HTML("Browse"),
                                                             choices = c("", unique(cdata[["donor.type"]])), selected = NULL,
                                                             options = list(placeholder = "all donors in database"), width = 220)
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
                                                        uiOutput(ns("abspanel"))
                                          )),
                                 fluidRow(
                                   shinycssloaders::withSpinner(plotlyOutput(ns("cases"), height = 750), color = "gray")
                            )),
                        tabPanel("By data variables",
                                 br(),
                                 div(class = "forceInline", uiOutput(ns("info2"))),
                                 div(class = "forceInline", downloadButton(ns("download"), label = "Download Collection")),
                                 DT::DTOutput(ns("table"))
                        )
            )
  )
}

#' Shiny module server functions for browsing metadata
#'
#' Handles general browsing, filtering, subsetting of table data
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param dt_id Table data by ID.
#' @param dt_var Table data by var.
#' @param inforRmd1 Helpfile for uploading an ID list.
#' @param inforRmd2 Helpfile for how browsing data works.
#' @export
browse <- function(input, output, session,
                   dt_id = cdata, dt_var = metadata,
                   infoRmd1 = system.file("help/ID_list.Rmd", package = "DIVE"),
                   infoRmd2 = system.file("help/browse_data.Rmd", package = "DIVE")) {


  visIDs <- reactiveVal(1:40)
  dataview <- reactiveVal(dt_id)
  customIDs <- reactiveVal(NULL)

  uploadedIDs <- callModule(dataUpload, "IDlist", asDT = F, removable = T, infoRmd = infoRmd1)

  observeEvent(uploadedIDs(), {
    if(is.null(uploadedIDs())) customIDs(NULL) else customIDs(uploadedIDs())
  }, ignoreNULL = F)

  observeEvent(input$submitID, {
    customIDs(input$selectID)
  })

  observeEvent(input$subset, {
    if(input$subset == "") dataview(dt_id) else dataview(dt_id[donor.type == input$subset])
    updateSelectizeInput(session, "selectID", selected = "")
    end <- ifelse(nrow(dataview()) < 40, nrow(dataview()), 40)
    visIDs(1:end)
  })

  observeEvent(customIDs(), {
    if(is.null(customIDs())) dataview(dt_id) else dataview(dt_id[ID %in% customIDs()])
    end <- ifelse(nrow(dataview()) < 40, nrow(dataview()), 40)
    visIDs(1:end)
  }, ignoreNULL = F)

  output$index <- renderPrint({
      helpText(paste0("displaying ", visIDs()[1], "-", visIDs()[length(visIDs())], " of ", nrow(dataview())))
  })

  observeEvent(input$nextSet, {
    if(last(visIDs()) < nrow(dataview())) {
      newrange <- visIDs() + 40
      if(last(newrange) > nrow(dataview())) newrange <- newrange[newrange <= nrow(dataview())]
      visIDs(newrange)
    }
  })

  observeEvent(input$prevSet, {
    if(visIDs()[1] != 1) visIDs(visIDs() - 40)
  })

  observeEvent(input$closepanel, {
    updateSelectInput(session, "expandon", selected = "")
  })

  varL <- reactive({
    IDs <- visIDs()
    varL <- dataview()[IDs, dataview()[IDs, sapply(.SD, function(x) !all(is.na(x))), with = F] ]

    for(col in removeID(names(varL))) varL[[col]] <- as.integer(!is.na(varL[[col]]))
    varL <- melt(varL, id.var = "ID", variable.name = "VarID", value.name = "Available")
    varL <- merge(varL, dt_var[, c("VarID", "Variable", input$class), with = F], by = "VarID")
    setnames(varL, input$class, "Data")
  })

  withclass <- reactive({
    unique(varL()[, .(ID, Available, Data)])
  })

  output$cases <- renderPlotly({
    withclass <- withclass()
    p <- plot_ly(withclass, x = ~Data, y = ~ID, name = "Available", type = "scatter", mode = "markers", source = "main",
                 marker = list(size = ~Available *6, color = "#404040", opacity = 1, line = list(color = "#404040")),
                 showlegend = FALSE) %>%
      layout(xaxis = list(type = "category", title = "Data", showline = FALSE, showgrid = FALSE),
             yaxis = list(type = "category", title = "Case", showline = FALSE, dtick = 1, tickfont = list(size = 9), showgrid = FALSE)) %>%
      event_register("plotly_click")


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
            nrows = 2, shareX = T, shareY = T,
            widths = c(0.9, 0.1), heights = c(0.2, 0.8))

  })

  output$expandUI <- renderUI({
    tags$div(
      div(class = "forceInline", selectInput(session$ns("expandon"), HTML("<i class='fas fa-expand'></i>&nbsp;&nbsp;<strong>Expand on</strong>"),
                                             choices = c("", unique(withclass()$Data)), selected = ""))
    )
  })

  output$abspanel <- renderUI({
    req(input$expandon)
    if(input$expandon != "") {
      div(class = "subgroups-panel", style = "background-color: WhiteSmoke;",
        div(align = "right", actionButton(session$ns("closepanel"), "", icon = icon("times"))),
        plotlyOutput(session$ns("expandplot"), height = 750, width = 500))
    }
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

  # Tab 2 ------------------------------------------------------------------------------------------------------------------------------------------------#

  # Optional info link  ------------------------------------------------------- #
  if(!is.null(infoRmd2)) {
    output$info2 <- renderUI({
      infoOutput(session$ns("browse"), label = "Key", i = "question-circle", link = F)
    })
    modal <- callModule(info, "browse", infoRmd = infoRmd2)
  }


  output$table <- DT::renderDataTable({
    show <- dt_var[, .(Source, Contributor, Variable, Description, IndividualLevelData, InApp, DataSource, DataSourceLink)]
    # if(input$filterDT) {
    #   show <- show[IndividualLevelData == "Yes", ]
    # }
    show[DataSourceLink != "", DataSourceLink := paste0("<a href='",DataSourceLink,"' target='_blank'",
                                                        " title='",DataSourceLink,"'><i class='fas fa-external-link-alt'></i></a>")]

  }, escape = FALSE, rownames = F, filter = "none", options = list(dom = 'ftp', pageLength = 10), style = "bootstrap")

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
