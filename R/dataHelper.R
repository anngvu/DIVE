#' Shiny app UI for simple interactions between two relational tables
#'
#' UI to traverse data in "righthand" and "lefthand" tables with additional attribute filters
#'
#' @family dataHelper
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @param theme Optional, name of theme for \code{shinythemes}.
#' @import shiny
#' @export
dataHelperUI <- function(id, CSS = "app.css", theme = "paper") {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme(theme),
            if(!is.null(CSS)) includeCSS(CSS),
            fluidRow(style = "margin-top: 20px",
              column(12,
                     div(class = "ui-inline",
                         # Filter panel
                         conditionalPanel("input.show % 2 == 0", ns = ns, class = "ui-inline",
                           div(class ="ui-inline card-panel data-helper-filter",
                               uiOutput(ns("lhfilter"))),
                           icon("arrow-right", "fa-3x")
                         )
                      ),

                     # Left-hand panel
                     div(class ="ui-inline card-panel", icon("id-card", "fa-2x"), style = "width: 20vw",
                         div(class ="ui-inline",
                           selectizeInput(ns("lhselect"), label = NULL, choices = "", multiple = TRUE, width = "100%",
                                          options = list(placeholder = "no results")),
                           verbatimTextOutput(ns("lhsummary")),
                           actionLink(ns("show"), label = "Upload ID list",
                                      title = "useful for custom lookup of a large number of IDs"),
                           conditionalPanel("input.show % 2 == 1", ns = ns,
                                            helpText("Use a .txt file with one ID per line"),
                                            dataUploadUI(ns("loadID"), label = NULL, buttonLabel = icon("upload"), width = "100%")
                           ), br(),
                           downloadLink(ns("saveID"), "Export subset to file")
                         ),
                     ),
                     div(class ="ui-inline", actionLink(ns("right2left"), label = NULL, icon = icon("arrow-right", "fa-3x"),
                                                        title = "reverse the direction of search")),

                     # Right-hand panel
                     div(class = "ui-inline card-panel", icon("folder", "fa-2x"), style = "width: 50vw",
                         selectizeInput(ns("rhselect"), label = NULL, choices = "", multiple = TRUE, width = "100%"),
                         uiOutput(ns("rhtableopts")),
                         DT::DTOutput(ns("rhtable"))
                     )
              )
            )
        )
}

#' Shiny module server function for simple interactions between two relational tables
#'
#' Implement ability to traverse data in "righthand" and "lefthand" tables with additional attribute filters
#'
#' This implements a simple interface to alllow general browsing, filtering, subsetting of traditional tabular data,
#' limited to two tables. The module requires these tables:
#' The "lefthand" table of something with attributes. In the original context, this was a table
#' of case IDs with attributes such as age and sex.
#' The "righthand" table of something with other attributes. In the original context,
#' this was a table of measurements with descriptions and references.
#' The lookup table to translate relations (use for joins) between the lefthand and righthand tables.
#'
#' @family dataHelper
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param lhdata A \code{data.table} of the dataset on the left-hand side.
#' @param lhdatakey Name of unique key column in \code{lhdata}, defaulting to "ID".
#' @param rhdata A \code{data.table} of the dataset on the right-hand side.
#' @param rhdatakey Name of unique key column in \code{rhdata}, defaulting to "Source".
#' @param initcolselect Initialize column selection (with a checkbox ui) with a vector of names
#' matching cols in \code{rhdata} to be shown. If \code{NULL}, column selection ui is not rendered.
#' @param handler A \code{data.table} used for translating keys between \code{lhdata} and \code{rhdata}.
#' @param filterconf If given, will create filters for the left-hand dataset using \code{\link{sideFilterUI}}.
#' Should be a list with names matching columns in \code{lhdata} to be used as filters
#' amd elements "type" with their input widget types, "selected" for the initial selection,
#' and "conditional" with a boolean value for whether this is a less important filter that should initially be hidden/ignored.
#' @import shiny
#' @export
dataHelperServer <- function(id,
                             lhdata, lhdatakey = "ID",
                             rhdata, rhdatakey = "Source",
                             initcolselect = NULL,
                             handler, filterconf) {

    moduleServer(id, function(input, output, session) {

      updateSelectizeInput(session, "lhselect", label = lhdatakey, choices = lhdata[[lhdatakey]], selected = c())
      updateSelectizeInput(session, "rhselect", label = rhdatakey, choices = rhdata[[rhdatakey]], selected = c(),
                           options = list(placeholder = "no results"))


      # Each filter col will have an input component to implement filtering
      filtercols <- names(lhdata)[names(lhdata) != lhdatakey]
      # Not all filter cols have to be used; it may be easier for user to be able to ignore certain ones
      switchfiltercols <- sapply(filtercols, function(x) paste("usefilter", gsub(".", "", x, fixed = T), sep = "_"))
      activefiltercols <- reactive({
        active <- unlist(sapply(switchfiltercols, function(x) input[[x]]))
        if(is.null(active) || all(active == F)) return(NULL) else filtercols[active]
      })

      # Convert links for display
      for(col in names(rhdata)) {
        if(any(grepl("^http:", rhdata[[col]]))) {
          rhdata[[col]] <- paste0("<a href='",rhdata[[col]],"' target='_blank'",
                                  "<i class='fas fa-external-link-alt'></i></a>")
        }
      }

      uploadedIDs <- dataUploadServer("loadID", asDT = F, removable = T)

      observeEvent(uploadedIDs(), {
        updateSelectizeInput(session, "lhselect", selected = uploadedIDs())
      }, ignoreNULL = F, ignoreInit = TRUE)


      # Render filtercols inputs
      output$lhfilter <- renderUI({
        uilist <- lapply(filtercols,
                         function(col) sideFilterUI(col, lhdata[[col]],
                                                    filterconf[[col]]$input,
                                                    filterconf[[col]]$selected,
                                                    ns = session$ns,
                                                    conditional = filterconf[[col]]$conditional))
        tagList(uilist)
      })


      # Process filtercols inputs
      obs_filters <- observe({
        if(is.null(activefiltercols())) {
          updateSelectizeInput(session, "lhselect", selected = "")
        } else {
          ids <- lapply(activefiltercols(), function(col) filter4j(lhdata, col, input[[paste0("filter-", col)]],
                                                                   lhdatakey, filterconf[[col]]$input))
          ids <- Reduce(intersect, ids)
          updateSelectizeInput(session, "lhselect", selected = ids)
        }
      })


      # Render left-hand summary
      output$lhsummary <- renderPrint({
        n <- length(input$lhselect)
        ntotal <- length(lhdata[[lhdatakey]])
        cat(n, "of", ntotal, "selected")
      })


      # Translate left-hand to matches in right-hand
      obs_left2right <- observeEvent(input$lhselect, {
        rhkeys <- handler[get(lhdatakey) %in% input$lhselect, get(rhdatakey)]
        updateSelectizeInput(session, "rhselect", selected = rhkeys)
      }, ignoreNULL = FALSE)


      # Render column selection
      output$rhtableopts <- renderUI({
        if(!is.null(initcolselect)) {
          checkboxGroupInput(session$ns("rhcols"), NULL, choices = names(rhdata), selected = initcolselect, inline = T)
        }
      })


      # Render right-hand table
      output$rhtable <- DT::renderDT({
        cols <- if(!is.null(input$rhcols)) input$rhcols else names(rhdata)
        rhdata[get(rhdatakey) %in% input$rhselect, ..cols]
      }, escape = F, rownames = F, filter = "none", options = list(dom = 'tp', pageLength = 10),
      style = "bootstrap")


      # Modify observers accordingly when switching from left-to-right or right-to-left
      observeEvent(input$right2left, {
        if(input$right2left %% 2 == 1) {
          obs_left2right$suspend()
          obs_right2left$resume()
          updateActionLink(session, "right2left", icon = icon("arrow-left", "fa-3x"))
        } else {
          obs_right2left$suspend()
          obs_left2right$resume()
          updateActionLink(session, "right2left", icon = icon("arrow-right", "fa-3x"))
        }
      }, ignoreInit = TRUE)


      # Translate right-hand to matches in left-hand
      obs_right2left <- observeEvent(input$rhselect, {
        lhkeys <- handler[get(rhdatakey) %in% input$rhselect, get(lhdatakey)]
        updateSelectizeInput(session, "lhselect", selected = lhkeys)
      }, suspended = TRUE, ignoreNULL = FALSE) # since left2right is the default on startup


      output$saveID <- downloadHandler(
        filename = "ID_data.tsv",
        content = function(file) {
          lhx <- lhdata[get(lhdatakey) %in% input$lhselect,
                        lapply(.SD, function(x) sapply(x, function(xx) paste(xx, sep = ","))), .SDcols = names(lhdata)]
          fwrite(lhx, file = file, sep = "\t")
      }, contentType = "text/csv")

    })
}

#' Helper function for extracting inputs created with \code{\link{sideFilterUI}}
#'
#' Get values stored in inputs created by \code{\link{sideFilterUI}}
#'
#' @param dt A \code{data.table}.
#' @param col Filter column in \code{dt}.
#' @param values The filter value(s) in `col`.
#' @param j The column of data to return for matches of `values` in `col`.
#' @param type Type of input filter; only "range" is special and requires `values` to be a 2-element vector.
filter4j <- function(dt, col, values, j, type) {
  if(type == "range") {
    dt[get(col) >= values[1] & get(col) <= values[2], get(j)]
  } else {
    if(!length(values) || class(dt[[col]]) == "list") {
      dt[get(col) %in% list(values), get(j)]
    } else {
      dt[get(col) %in% values, get(j)]
    }
  }
}

#' Generate a combination of filters
#'
#' Convenient creation of a set of filters using selected inputs
#'
#' A wrapper for quick creation of Shiny inputs, though limited to certain input types.
#' Inputs are intended to be used for filtering table data, e.g. each column of a table gets its
#' own specified input component, which can be of a type enumerated in the parameter \code{type}.
#' Generally, \code{sliderInput} (range) is appropriate for numeric columns representing continuous data.
#' while \code{selectInput}, \code{selectizeInput}, \code{checkboxInput} can be used for discrete data,
#' and \code{checkboxGroupInput} can be used for list columns (if using \code{data.table}).
#'
#' Whether to initialize the input with none or all of the available values selected will
#' depend on the data. Imagine a very big table that should at first only show the most relevant
#' filtered subset -- in this case (and most commonly) it makes the most sense to initialize inputs with
#' a selection of values that does not lead to rendering the whole table.
#'
#' The inputs have namespaced IDs "<namespace>-filter-<colname>" by applying the passed-in
#' namespacing function \code{ns} to \code{id}. To get values in a server function, use \code{\link{filter4j}}.
#' @param inputId The input slot that will be used to access the value.
#' @param values Choices for the input, depends on input type.
#' @param type One of \code{c("select", "selectize", "checkbox", "checkboxGroup", "range")}, corresponding to
#' \code{selectInput}, \code{selectizeInput}, \code{checkboxInput}, \code{checkboxGroupInput}, or \code{sliderInput} (range).
#' @param selected The initial selection of the input, defaulting to the first of \code{values}.
#' @param conditional Optional. If not `NULL`, wrap element in a conditional panel with inital display given by the boolean value.
#' @param ns Function for namespacing components, i.e. from `session$ns`.
#' @param width Width of input elements.
sideFilterUI <- function(inputId, values,
                         type = c("select", "selectize", "checkbox", "checkboxGroup", "range"),
                         selected = c("first", "last", "all", "none"),
                         conditional = NULL,
                         ns = NS(NULL),
                         width = 200) {

  id <- ns(paste0("filter-", inputId))

  if(type == "range") {
    ui <- sliderInput(id, NULL, min = min(values, na.rm = T), max = max(values, na.rm = T),
                  value = c(min, max), width = width)
  } else {
    # select, selectize, checkbox, checkboxGroup
    choices <- unique(unlist(values))
    selected <- switch(match.arg(selected),
                       first = first(choices),
                       last = last(choices),
                       all = choices,
                       none = NULL)
    ui <- do.call(paste0(type, "Input"), list(inputId = id, label = NULL, choices = choices, selected = selected, width = width))
  }
  if(!is.null(conditional)) {
    actid <- paste("usefilter", gsub(".", "", id, fixed = T), sep = "_")
    ui <- div(shinyWidgets::prettySwitch(ns(actid), NULL, value = conditional, slim = TRUE, inline = TRUE),
              tags$span(id, class = "filter-label"), br(),
              conditionalPanel(paste0("input.",actid), ui, ns = ns, class = "ui-inline"))
  } else {
    ui <- div(h5(id), ui)
  }
  ui
}


dataHelperApp <- function(ns, ...) {
  ui <- dataHelperUI(ns)
  server <- function(input, output, session) { dataHelperServer(ns, ...) }
  shinyApp(ui = ui, server = server)
}