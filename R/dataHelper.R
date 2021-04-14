#' Shiny app UI for simple traversal of relational tables in a database
#'
#' UI to traverse data between "right-hand" and "left-hand" tables
#'
#' @family dataHelper
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param CSS Optional, location to an alternate CSS stylesheet to change the look and feel of the app.
#' @param theme Optional, name of theme for \code{shinythemes}.
#' @param oneway Whether data traversal should be one-way (left-to-right) only, or both ways.
#' Defaults to \code{FALSE}.
#' @param lefticon Optional, an icon to represent the left-hand data.
#' @param righticon Optional, an icon to represent the right-hand data.
#' @import shiny
#' @export
dataHelperUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE"), theme = "paper",
                         oneway = FALSE, lefticon = NULL, righticon = NULL) {

  ns <- NS(id)
  div(
    style = "display: flex;",
    # Left-hand panel
    div(
      class = "ui-inline card-panel",
      if(!is.null(lefticon)) icon(lefticon, "fa-2x"),
      div(class = "ui-inline",
          DT::DTOutput(ns("lhDT"))),
    ),

    # Directional switch
    div(
      class = paste(c("ui-inline", if(oneway) "hidden"), collapse = " "),
      shinyWidgets::radioGroupButtons(
        inputId = ns("handler"),
        choiceNames = c(
          HTML('<i class="fas fa-arrow-right fa-2x"></i>'),
          if (oneway)
            NULL
          else
            HTML('<i class="fas fa-arrow-left fa-2x"></i>')
        ),
        choiceValues = c("left2right", if (oneway)
          NULL
          else
            "right2left"),
        direction = "vertical"
      )
    ),

    # Right-hand panel
    conditionalPanel(
      "typeof input.lhDT_rows_selected  !== 'undefined' && input.lhDT_rows_selected.length > 0",
      ns = ns,
      class = "ui-inline card-panel",
      if(!is.null(righticon)) icon(righticon, "fa-2x"),
      DT::DTOutput(ns("rhDT"))
    )
  )
}

#' Shiny module server for simple traversal of relational tables in a database
#'
#' Traverse data in "right-hand" and "left-hand" tables
#'
#' Implements simple interface allowing constrained browsing, filtering, subsetting of traditional tabular data.
#' The module requires these tables:
#' \describe{
#'   \item{\code{lhdata}}{ "Left-hand" table of data. Conceptually, also known as a "dimension table".
#'   For example, a table of unique patients and attributes such as age, sex, race, etc. }
#'   \item{\code{rhdata}}{ "Right-hand" table of data. Conceptually, also known as a "dimension table".
#'   For example, a table of unique diagnoses and attributes such as description, insurance code, etc. }
#'   \item{\code{handler}}{ Lookup table to translate relations between the left-hand and right-hand tables.
#'   Using current example, a table linking patient IDs to diagnosis IDs (which can be many-to-many).
#'   See helpful reference for
#'   \href{https://www.ibm.com/support/knowledgecenter/SSEP7J_11.1.0/com.ibm.swg.ba.cognos.ug_fm.doc/c_dyn_query_bridge_tables.html}{bridge table}}.
#' }
#'
#'
#' @family dataHelper
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param dbcon A database connection object to the database containing the required tables.
#' @param lhdata Name of the table on the left-hand side.
#' @param lhdatakey Name of key column in \code{lhdata} table, which should also be a key in \code{handler}.
#' @param rhdata Name of the table on the right-hand side.
#' @param rhdatakey Name of key column in \code{rhdata} table, which should also be a key in \code{handler}.
#' @param handler Name of table used for translating between \code{lhdata} and \code{rhdata}.
#'
#' @import shiny
#' @export
dataHelperServer <- function(id,
                              dbcon = NULL,
                              lhdata, lhdatakey, # lhreact = FALSE,
                              rhdata, rhdatakey, # rhreact = FALSE,
                              handler) {

    moduleServer(id, function(input, output, session) {

      handler <- dplyr::tbl(dbcon, handler)

      # lhdata and/or rhdata can be reactive objects passed on by another module;
      # do not initialize using dbcon if this is the case
      lhdata <- dplyr::tbl(dbcon, lhdata)
      rhdata <- dplyr::tbl(dbcon, rhdata)

      # Defaults to left-to-right, so initialize with full LT
      LDT <- reactiveVal(lhdata)
      RDT <- reactiveVal(rhdata)

      # Left-hand components ---------------------------------------------------------------#

      output$lhDT <- DT::renderDT({
        LDT() %>% dplyr::collect()
      },
      escape = F, rownames = F, filter = "none", selection = "single",
      options = list(dom = 'tp', pageLength = 10, scrollX = TRUE),
      style = "bootstrap", class = "table-condensed table-hover")

      # Right-hand components ----------------------------------------------------------#

      # Render right-hand table
      output$rhDT <- DT::renderDT({
        RDT() %>% dplyr::collect()
      },
      escape = F, rownames = F, filter = "none",
      options = list(dom = 'tp', pageLength = 10, scrollX = TRUE),
      style = "bootstrap", class = "table-condensed table-hover")

      # Translate between ------------------------------------------------------------#

      # Translate left-hand to matches in right-hand; output displayed in right-hand table
      obs_left2right <- observeEvent(input$lhDT_rows_selected, {
        i <- input$lhDT_rows_selected
        if(is.null(i)) i <- 0L
        tabl <- lhdata %>%
          dplyr::filter(dplyr::row_number() %in% i) %>%
          dplyr::select(!!lhdatakey) %>%
          dplyr::inner_join(handler, by = lhdatakey) %>%
          dplyr::inner_join(rhdata, by = rhdatakey) %>%
          dplyr::select(-!!lhdatakey)
        RDT(tabl)
      }, suspended = FALSE, ignoreNULL = FALSE)

      # Translate right-hand to matches in left-hand; output displayed in left-hand table
      obs_right2left <- observeEvent(input$rhDT_rows_selected, {
        i <- input$rhDT_rows_selected
        if(is.null(i)) i <- 0L
        tabl <- rhdata %>%
          dplyr::filter(dplyr::row_number() %in% i) %>%
          dplyr::select(!!rhdatakey) %>%
          dplyr::inner_join(handler, by = rhdatakey) %>%
          dplyr::inner_join(lhdata, by = lhdatakey) %>%
          dplyr::select(-!!rhdatakey)
        LDT(tabl)
      }, suspended = TRUE, ignoreNULL = FALSE)

      # Modify observers accordingly when switching from left-to-right or right-to-left
      #
      observeEvent(input$handler, {
        if(input$handler == "left2right") {
          obs_right2left$suspend()
          obs_left2right$resume()
          LDT(lhdata)
        } else {
          obs_left2right$suspend()
          obs_right2left$resume()
          RDT(rhdata)
        }

      })

      result <- reactive({
         if(input$handler == "left2right") RDT() %>% dplyr::collect() else LDT() %>% dplyr::collect()
      })


      return(result)

    })
}

dataHelperApp <- function(ns, ...) {
  ui <- dataHelperUI(ns)
  server <- function(input, output, session) { dataHelperServer(ns, ...) }
  shinyApp(ui = ui, server = server)
}
