#' Shiny app UI for browsing metadata
#'
#' Shiny app UI for browsing metadata
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @param theme Optional, name of theme for shinythemes.
#' @export
browseUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE"), theme = "paper") {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme(theme),
            if(!is.null(CSS)) includeCSS(CSS),
            tabsetPanel(id = ns("tabs"),
                        tabPanel("Metadata Map",
                                 fluidRow(style = "margin-top: 20px; margin-left: 20px;", height = 100,
                                          div(class = "forceInline", style = "background-color: WhiteSmoke;",
                                              div(class = "forceInline", style = "margin-left: 20px;",
                                                  uiOutput(ns("annotation"))
                                              ),
                                              div(class = "forceInline",
                                                  div(class = "forceInline", uiOutput(ns("expandUI")))
                                              )),
                                          div(class = "forceInline", br(),
                                              actionButton(ns("prevSet"), "Prev 40"),
                                              actionButton(ns("nextSet"), "Next 40"),
                                              htmlOutput(ns("index"))
                                          ),
                                          div(class = "forceInline",
                                              uiOutput(ns("selectbyID"))
                                              ),
                                          div(class = "forceInline", br(),
                                              actionButton(ns("submit"), "go")
                                          ),
                                          div(class = "forceInline",
                                              dataUploadUI(ns("IDlist"), label = "Or upload a list of IDs",
                                                           buttonlabel = "Upload", width = 200)
                                          ),
                                          div(class = "forceInline", style = "margin-left: 10px;",
                                             uiOutput(ns("selectbygroup"))
                                          )
                                 ),
                                 fluidRow(absolutePanel(style = "z-index: 10;", draggable = T,
                                                        uiOutput(ns("abspanel"))
                                          )),
                                 fluidRow(
                                   shinycssloaders::withSpinner(plotlyOutput(ns("cases"), height = 750), color = "gray")
                            )),
                        tabPanel("Metadata Dictionary",
                                 br(),
                                 div(class = "forceInline", uiOutput(ns("info2"))),
                                 DT::DTOutput(ns("table"))
                        ),
                        tabPanel("SPARQL Query",
                                 br(),
                                 "The SPARQL Query portal allows advanced searching and filtering with metadata.",
                                 br(),
                                 textOutput(ns("SPARQLsrvstat"))
                        )
            )
  )
}

#' Shiny module server functions for browsing metadata
#'
#' Handles general browsing, filtering, subsetting of metadata table. The module requires two tables:
#' a table of ID and all measured features, and a table of those measured features and metadata columns.
#' Generally, users can select specific IDs or a group of IDs and see what features are available.
#' A join of the two input tables on the feature name allows comparing IDs vs. selected metadata.
#'
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param dt_id Table data by ID.
#' @param index1 Name of ID column that serves as primary index.
#' @param index2 Optional, name of another column for secondary index.
#' @param dt_var Table data by var/feature.
#' @param dt_var_ext Extended table to provide lookup details of metadata.
#' @param informd1 Helpfile for uploading an ID list.
#' @param informd2 Helpfile for how browsing data works.
#' @export
browse <- function(input, output, session,
                   dt_id = NULL, index1 = "ID", index2 = NULL,
                   dt_var = NULL, dt_var_index = NULL,
                   dt_var_ext = NULL,
                   informd1 = system.file("help/ID_list.Rmd", package = "DIVE"),
                   informd2 = system.file("help/browse_data.Rmd", package = "DIVE")) {

  setkeyv(dt_id, index1)
  setindexv(dt_id, index2)

  visIDs <- reactiveVal(1:40)
  dataview <- reactiveVal(dt_id)
  customIDs <- reactiveVal(NULL)

  uploadedIDs <- callModule(dataUpload, "IDlist", asDT = F, removable = T, informd = informd1)

  # Available filters
  output$selectbyID <- renderUI({
    selectizeInput(session$ns("selectID"), "Select by specific IDs",
                   choices = c("", unique(dt_id[[index1]])), selected = NULL, multiple = T,
                   options = list(placeholder = "(max 10)",
                                  maxItems = 10), width = 220)
  })

  output$selectbygroup <- renderUI({
    if(!is.null(index2)) {
      selectizeInput(session$ns("subset"), "Browse",
                   choices = c("", unique(dt_id[[index2]])), selected = NULL,
                   options = list(placeholder = "all in database"), width = 220)
    }
  })

  output$annotation <- renderUI({
    selectInput(session$ns("class"), HTML("<strong>Data annotation</strong>"),
                choices = names(dt_var)[names(dt_var) != dt_var_index], width = 200)
  })

  output$expandUI <- renderUI({
      req(input$class)
      selectInput(session$ns("expandon"), HTML("<strong>Expand on</strong>"),
                  choices = c("", unique(id_data_map()$Data)), selected = "")

  })

  observeEvent(uploadedIDs(), {
    if(is.null(uploadedIDs())) customIDs(NULL) else customIDs(uploadedIDs())
  }, ignoreNULL = F)

  observeEvent(input$submit, {
    customIDs(input$selectID)
  })

  observeEvent(input$subset, {
    if(input$subset == "") dataview(dt_id) else dataview(dt_id[.(input$subset), on = index2])
    updateSelectizeInput(session, "selectID", selected = "")
    end <- ifelse(nrow(dataview()) < 40, nrow(dataview()), 40)
    visIDs(1:end)
  })

  observeEvent(customIDs(), {
    if(is.null(customIDs())) dataview(dt_id) else dataview(dt_id[customIDs()])
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

  #
  id_data_map <- reactive({
    req(dataview(), visIDs(), input$class)
    IDs <- visIDs()
    # Show only variables that were measured for at least one of the selected IDs
    id_data_map <- dataview()[IDs, dataview()[IDs, sapply(.SD, function(x) !all(x == 0)), with = F] ]
    # convert the dt_id table into a binary version
    # TO DO: if dt_id is very large, this could slow things down considerably,
    # so this needs to be calculated only once at beginning instead of within reactive
    for(col in names(id_data_map)[names(id_data_map) != index1]) id_data_map[[col]] <- as.integer(!is.na(id_data_map[[col]]))
    if(nrow(id_data_map) == 0) { # if selected IDs have no measurements of interest
      return(NULL)
    } else {
      id_data_map <- melt(id_data_map, id.var = "ID", variable.name = "VarID", value.name = "Available")
      # Join on variable name allows display of ID vs. selected metadata
      id_data_map <- merge(id_data_map, dt_var[, c(dt_var_index, input$class), with = F], by.x = "VarID", by.y = dt_var_index)
      setnames(id_data_map, input$class, "Data")
      id_data_map
    }
  })

  output$cases <- renderPlotly({
    req(id_data_map())
    withclass <- unique(id_data_map()[, .(ID, Data, Available)])
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
      orientation = "h", # must be explicit
      marker = list(color = "#2196f3"),
      showlegend = FALSE
    )  %>% plotly::config(displayModeBar = F)

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

  output$abspanel <- renderUI({
    req(input$expandon)
    if(input$expandon != "") {
      div(class = "subgroups-panel",
        div(align = "right", actionButton(session$ns("closepanel"), "", icon = icon("times"))),
        plotlyOutput(session$ns("expandplot"), height = 750, width = 500))
    }
  })

  output$expandplot <- renderPlotly({
    if(!length(input$expandon)) return()
    xvars <- id_data_map()[Data %in% input$expandon, .(VarID, ID, Available)]
    plot_ly(xvars, x = ~VarID, y = ~ID, type = "scatter", mode = "markers",
            marker = list(size = ~Available *6, color = "#404040", opacity = 1, line = list(color = "#404040")),
            showlegend = FALSE) %>%
      # to do: long x-axis labels seems to make plot die
      layout(xaxis = list(type = "category", title = "Data", showline = FALSE, showgrid = FALSE, showticklabels = F),
             yaxis = list(type = "category", title = "ID", showline = FALSE, dtick = 1, tickfont = list(size = 9), showgrid = FALSE),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent") %>%
      plotly::config(displayModeBar = F)
  })

  # Tab 2 ------------------------------------------------------------------------------------------------------------------------------------------------#

  # Optional info link  ------------------------------------------------------- #
  if(!is.null(informd2)) {
    output$info2 <- renderUI({
      infoOutput(session$ns("browse"), label = "Key", i = "question-circle", link = F)
    })
    modal <- callModule(info, "browse", informd = informd2)
  }


  output$table <- DT::renderDataTable({
    # automatically convert links for display
    for(col in names(dt_var_ext)) {
      if(any(grepl("^http:", dt_var_ext[[col]]))) dt_var_ext[[col]] <- paste0("<a href='",dt_var_ext[[col]],"' target='_blank'",
                                                                              " title='",dt_var_ext[[col]],"'><i class='fas fa-external-link-alt'></i></a>")
    }
    dt_var_ext

  }, escape = FALSE, rownames = F, filter = "none", options = list(dom = 'ftp', pageLength = 10), style = "bootstrap")

  # Tab 3 ------------------------------------------------------------------------------------------------------------------------------#

  # to do: code to communicate with SPARQL server
  SPARQLsrv <- reactive({
    NULL
  })

  output$SPARQLsrvstat <- renderPrint({
    status <- if(is.null(SPARQLsrv())) "down" else "available"
    paste("Server status:", status)
  })

}

#' Launch Shiny app for browsing metadata
#'
#' Wrapper to launch app at console
#'
#' @export
browseR <- function(ns, ...) {
  ui <- browseUI(ns)
  server <- function(input, output, session) { callModule(browse, ns, ...) }
  shinyApp(ui = ui, server = server)
}
