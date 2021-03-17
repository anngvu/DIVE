#' Shiny app UI for simple search and filtering of relational tables in a database
#'
#' UI to traverse data in "right-hand" and "left-hand" tables with additional attribute data
#'
#' @family dataHelper2
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @param theme Optional, name of theme for \code{shinythemes}.
#' @import shiny
#' @export
dataHelper2UI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE"), theme = "paper") {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme(theme),
            includeScript("www/app.js"),
            if(!is.null(CSS)) includeCSS(CSS),
            div(style = "display: flex;",
                div(class ="ui-inline card-panel data-helper-filter", style = "width: 280px",
                    tabsetPanel(type = "pills",
                                     tabPanel(title = "Attributes", br(),
                                      filterPanel2UI(ns("lh"))),
                                     tabPanel(title = "IDs", br(),
                                      helpText("Use a .txt file with one ID per line"),
                                      dataUploadUI(ns("loadID"), label = NULL, buttonLabel = icon("upload"), width = "170px"))
                          )),
                icon("arrow-right", "fa-3x"),

                 # Left-hand panel
                 div(class ="ui-inline card-panel", icon("id-card", "fa-2x"), style = "width: 600px",
                     div(class ="ui-inline",
                       DT::DTOutput(ns("lhtable"), width = "540px"),
                       downloadLink(ns("saveLT"), "Save records to file")
                     ),
                 ),

                 div(class = "ui-inline",
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("handler"),
                       choiceNames = c(HTML('<i class="fas fa-unlink fa-2x"></i>'),
                                       HTML('<i class="fas fa-hand-point-right fa-2x"></i>'),
                                       HTML('<i class="fas fa-hand-point-left fa-2x"></i>')),
                       choiceValues = c("unlink", "left2right", "right2left"),
                       direction = "vertical"
                 )),

                 # Right-hand panel
                 div(class = "ui-inline card-panel", icon("folder", "fa-2x"), style = "width: 600px",
                     DT::DTOutput(ns("rhtable"), width = "540px")
                 ),

                 icon("arrow-left", "fa-3x"),
                 div(class ="ui-inline card-panel data-helper-filter", style = "width: 280px",
                     filterPanel2UI(ns("rh"))
                 )
              )
        )
}

#' Shiny module server function for simple search and filtering of relational tables in a database
#'
#' Implement interface to traverse data in "right-hand" and "left-hand" tables with additional attribute filters
#'
#' The simple interface implemented allows constrained browsing, filtering, subsetting of traditional tabular data.
#' The module requires these tables:
#' \describe{
#'   \item{\code{lhdata}}{ "Left-hand" table of data. Conceptually it is also known as a "dimension table".
#'   Originally, this was a table of case IDs with columns for attributes such as age and sex. }
#'   \item{\code{rhdata}}{ "Right-hand" table of data. Conceptually it is also known as a "dimension table".
#'   Originally, this was a table of studies with columns for attributes such as authors, description and reference.}
#'   \item{\code{handler}}{ Lookup table to translate relations between the left-hand and right-hand tables.
#'   See helpful reference for \href{https://www.ibm.com/support/knowledgecenter/SSEP7J_11.1.0/com.ibm.swg.ba.cognos.ug_fm.doc/c_dyn_query_bridge_tables.html}{bridge table}}.
#' }
#'
#'
#' @family dataHelper2
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param dbcon A database connection object to the database with the required tables.
#' @param lhdata Name of the table on the left-hand side.
#' @param lhdatakey Name of unique key column in \code{lhdata} table, defaulting to "ID".
#' @param lhfilterconf If given, will create filters for the left-hand dataset using \code{\link{sideFilterUI}}.
#' Each named list should have name match a column \code{lhdata} and
#' also named configuration values which are passed to \code{\link{sideFilterUI}}:
#' \describe{
#'   \item{\code{input}}{ One-character vector specifying input type. }
#'   \item{\code{selected}}{ Initial selection type. }
#'   \item{\code{conditional}}{ Boolean value for whether this is a filter that should initially be shown or hidden. }
#' }
#' @param rhdata Name of the table on the right-hand side.
#' @param rhdatakey Name of unique key column in \code{rhdata}, defaulting to "ID".
#' @param rhfilterconf See \code{lhfilterconf}.
#' @param handler Name of table used for translating foreign keys between \code{lhdata} and \code{rhdata}.
#'
#' @import shiny
#' @export
dataHelper2Server <- function(id,
                              dbcon = NULL,
                              lhdata, lhdatakey = "ID", lhfilterconf,
                              rhdata, rhdatakey = "ID", rhfilterconf,
                              handler) {

    moduleServer(id, function(input, output, session) {

      if(!is.null(dbcon)) {
        lhdata <- dplyr::tbl(dbcon, lhdata)
        rhdata <- dplyr::tbl(dbcon, rhdata)
        handler <- dplyr::tbl(dbcon, handler)
      }

      filteredLT <- reactiveVal(NULL)
      filteredRT <- reactiveVal(NULL)

      # Input ID list option ------------------------------------------------------------------#
      uploadedIDs <- dataUploadServer("loadID", asDT = F, removable = T)

      observeEvent(uploadedIDs(), {
        tabl <- lhdata %>% dplyr::filter(!!lhdatakey %in% uploadedIDs) %>% dplyr::collect()
        filteredRT(tabl)
      }, ignoreNULL = F, ignoreInit = TRUE)

      # Left-hand filter selection ----------------------------------------------------------#

      filteredL <- filterPanel2Server("lh", dt = lhdata, dtkey = NULL, filterconf = lhfilterconf)

      observeEvent(filteredL(), {
        filteredLT(filteredL() %>% dplyr::collect())
      })

      # Left-hand components ---------------------------------------------------------------#

      output$lhtable <- DT::renderDT({
        filteredLT()
      }, escape = F, rownames = F, filter = "none", options = list(dom = 'tp', pageLength = 10, scrollX = TRUE), style = "bootstrap")

      output$saveLT <- downloadHandler(
        filename = "records.tsv",
        content = function(file) {
          data.table::fwrite(filteredLT(), file = file)
      }, contentType = "text/csv")

      # Right-hand filter selection ----------------------------------------------------------#

      filteredR <- filterPanel2Server("rh", dt = rhdata, dtkey = NULL, filterconf = rhfilterconf)

      observeEvent(filteredR(), {
        filteredRT(filteredR() %>% dplyr::collect())
      })

      # Right-hand components ----------------------------------------------------------#

      # Render right-hand table
      output$rhtable <- DT::renderDT({
        filteredRT()
      }, escape = F, rownames = F, filter = "none", options = list(dom = 'tp', pageLength = 10, scrollX = TRUE), style = "bootstrap")

      # Translate between ------------------------------------------------------------#

      # Translate left-hand to matches in right-hand; output displayed in right-hand table
      obs_left2right <- observeEvent(filteredL(), {
        req(filteredL())
        tabl <- filteredL() %>%
          dplyr::select(!!lhdatakey) %>%
          dplyr::inner_join(handler, by = lhdatakey) %>%
          dplyr::inner_join(rhdata, by = rhdatakey) %>%
          dplyr::collect()
        filteredRT(tabl)
      }, suspended = TRUE, ignoreNULL = FALSE)

      # Translate right-hand to matches in left-hand; output displayed in left-hand table
      obs_right2left <- observeEvent(filteredR(), {
        req(filteredR())
        tabl <- filteredR() %>%
          dplyr::select(!!rhdatakey) %>%
          dplyr::inner_join(handler, by = rhdatakey) %>%
          dplyr::inner_join(lhdata, by = lhdatakey) %>%
          dplyr::collect()
        filteredLT(tabl)
      }, suspended = TRUE, ignoreNULL = FALSE)

      # Modify observers accordingly when switching from left-to-right or right-to-left
      observeEvent(input$handler, {
        if(input$handler == "left2right") {
          obs_right2left$suspend()
          obs_left2right$resume()
        } else if(input$handler == "right2left") {
          obs_left2right$suspend()
          obs_right2left$resume()
        } else {
          obs_right2left$suspend()
          obs_left2right$suspend()
        }
      })

      # Advanced options ------------------------------------------------------------#
      # TO DO
      # output$query <- renderPrint({
      #   filteredL() %>% dplyr::show_query()
      # })

    })
}

#' Filter panel UI
#'
#' A panel with a combination of filter inputs
#'
#' Components are actually rendered using the function \code{sideFilterUI}
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
filterPanel2UI <- function(id) {
   ns <- NS(id)
   uiOutput(ns("filter"))
}

#' Filter server
#'
#' Render filter ui and process output from user interaction
#'
#' The server module dynamically renders a filter ui given the specification
#' passed in using \code{filterconf}. Because not all available filters need to be "active",
#' \code{filterconf} allows specifying which filters should be initially active
#' and which should be considered optional. Users can toggle which filters to apply.
#' Each input component filters upon values of columns in the table \code{dt}.
#' The applied combination is outputted as matching \code{dtkey} entries
#' (not entire rows). Calculation is actually delegated to the function \code{filter4j}.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param dt A database table.
#' @param dtkey Name of unique key column in \code{dt}, defaulting to "ID".
#' @param filterconf A list with names of columns in \code{dt} to be used as filters,
#' elements "input" with their input widget types, "selected" for the initial selection,
#' and "conditional" with a boolean value for whether
#' this is a filter that should initially be active/inactive.
filterPanel2Server <- function(id,
                               dt, dtkey = "ID",
                               filterconf) {

  moduleServer(id, function(input, output, session) {

    filteroutput <- reactiveVal(NULL)

    # Conf determines col with input component to implement filtering
    filtercols <- names(filterconf)

    # Not all filter cols have to be shown/used on init
    # switchfiltercols are the ids of switch inputs that allow users to activate a filter
    switchfiltercols <- sapply(filtercols, function(x) paste("usefilter", gsub(".", "", x, fixed = T), sep = "_"))

    # Names of active filter cols ("on" filters)
    activefiltercols <- reactive({
      active <- unlist(sapply(switchfiltercols, function(x) input[[x]]))
      if(is.null(active) || all(active == F)) return(NULL) else filtercols[active]
    })

    # Render filtercols inputs
    output$filter <- renderUI({
      # values need to be summarized for db table
      uilist <- lapply(filtercols,
                       function(col) {
                         values <- if(filterconf[[col]]$input == "range") colRange(tbl = dt, col) else colUniqueValues(tbl = dt, col)
                         sideFilterUI(inputId = col,
                                      values = values,
                                      input = filterconf[[col]]$input,
                                      selected = filterconf[[col]]$selected,
                                      conditional = filterconf[[col]]$conditional,
                                      ns = session$ns)
                       })
      tagList(uilist)
    })

    # Process filtercols inputs
    obs_filters <- observe({
      if(is.null(activefiltercols())) {
        filteroutput(NULL)
      } else {
        # Create filter expr for active filters, then apply all
        fs <- lapply(activefiltercols(), function(col) filterQuo(col, input[[paste0("filter-", col)]], filterconf[[col]]$input))
        result <- filterApply(dt, fs, j = dtkey)
        filteroutput(result)
      }
    })

    return(filteroutput)

  })
}


#' Generate filter expression for filter inputs
#'
#' Return a quosure of the appropriate filter expression
#' for the given input component type to use later on
#'
#' Most input components serve user selection/filter purposes;
#' \code{\link{sideFilterUI}} creates these input ui while this function
#' formulates the selection/filter expression from the input values.
#' In order to compose the appropriate expression,
#' we still need some info about the input component given by the parameter \code{type},
#' e.g. whether it is "range" for a range sliderInput or something else.
#'
#' @param col Filter column in \code{dt}.
#' @param values The filter value(s) in `col`.
#' @param type Type of input filter; only "range" is special and requires `values` to be a 2-element vector.
#'
#' @keywords internal
#' @importFrom rlang .data
filterQuo <- function(col, values, type) {
  if(type == "range") {
    minval <- values[1] # using values[1] directly in between gives error
    maxval <- values[2]
    quo(between( .data[[col]], minval, maxval))
  } else {
    quo(.data[[col]] %in% values)
  }
}


#' Apply filter expressions to data
#'
#' @param dt The data table.
#' @param fs Filter expression(s) as quosure or list of quosures.
#' @param j Optional, character name of column of data to return for matches of `values` in `col`.
#' @param res Result/resolution type.
#' If \code{NULL}, returns all columns for subsetted rows.
#'
#' @keywords internal
filterApply <- function(dt, fs, j = NULL, res = 0) {
  result <- dt %>% dplyr::filter(!!!fs)
  if(!is.null(j)) {
    result <- result %>% dplyr::select(!!j)
  }
  if(res == 1) result <- result %>% dplyr::collect() else result
  return(result)
}

#' Return unique values for a column in a table
#'
#' Return unique values for a column in a table
#'
#' This is a convenience function for reporting values/getting the selection of a Shiny input component.
#' @keywords internal
#' @importFrom rlang .data
colUniqueValues <- function(tbl, col) {
  tbl %>%
    dplyr::distinct(.data[[col]]) %>%
    dplyr::pull()
}

#' Return range for a column in a table
#'
#' Return range for a column in a table
#'
#' This is a convenience function for reporting range, e.g. as the selection of a Shiny range slider component.
#'
#' @param tbl Table of data.
#' @param col Name of column.
#' @keywords internal
#' @importFrom rlang .data
colRange <- function(tbl, col) {
  tbl %>%
    dplyr::summarize(min = min( .data[[col]], na.rm = T), max = max( .data[[col]], na.rm = T)) %>%
    dplyr::collect() %>% unlist()
}


dataHelper2App <- function(ns, ...) {
  ui <- dataHelper2UI(ns)
  server <- function(input, output, session) { dataHelper2Server(ns, ...) }
  shinyApp(ui = ui, server = server)
}
