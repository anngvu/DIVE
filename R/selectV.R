#' Shiny module UI for selecting columns from a data table
#'
#' A basic UI for choosing columns in a dataset.
#'
#' @family multiVApp module functions
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
selectVUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "selectVUI",
           uiOutput(ns("select"))
           )
}

#' Shiny module server for selecting columns from a data table
#'
#' Returns the selected columns.
#'
#' This is a subcomponent that originally serves under \code{\link{multiVCtrl}}
#' but should be generic enough to be integrated with other modules.
#' When data is uploaded through \code{\link{multiVCtrl}}, it can be "high-throughput"
#' or "low-throughput" type data, the latter typically as phenotype or clinical variables.
#' This module handles changes in the table of the latter data type.
#' When the user adds new data columns to the table, the selection changes to include these new options.
#' When the user selects specific columns from the menu, a column-subsetted table is returned.
#' The most recent modification involves adding parenthesized counts for each variable option,
#' i.e. "phenotypeA (15)", calculated on intersections with another table.
#'
#' @family multiVUI module functions
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param data A reactive data.table. See details.
#' @param key A key column that is kept for every selected subset. Defaults to "ID".
#' @param label Label for variable select input.
#' @param selected Optional, initial selection.
#' @param countby The matrix representing a high-throughput dataset with sample IDs,
#' which are intersected to generate counts in the select options. See details.
#' @param maxitems Maximum number of items that can be selected. Defaults to 3.
#' @return The subsetted data table.
#' @export
selectVServer <- function(id,
                          data = reactive({ NULL }), key = "ID",
                          label = HTML("<strong>Phenotype/Experimental variable(s)</strong>"),
                          selected = reactive({ NULL }),
                          countby = reactive({ NULL }),
                          maxitems = 3)  {

  moduleServer(id, function(input, output, session) {

    Vdata <- reactiveVal(NULL)

    output$select <- renderUI({
      choices <- names(data())[names(data()) != key]
      ids <- rownames(countby()[[1]])
      if(length(ids)) {
        counts <- colSums(!is.na(data()[get(key) %in% ids, choices, with = F]))
        counts <- sort(counts, decreasing = T)
        choices <- setNames(names(counts), paste0(names(counts), " (", counts, ")"))
      }
      tags$div(
        div(class = "forceInline",
            selectizeInput(session$ns("var"), label = label,
                           choices = choices, selected = selected(),
                           width = "450px", options = list(maxItems = maxitems))
            ),
        div(class = "forceInline",
            selectInput(session$ns("sortby"), "Sort by", choices = "", selected = "", width = "300px"))
        )
    })

    observeEvent(data(), {
      if(!is.null(selected())) Vdata( data()[, c(key, selected()), with = F] )
    })

    observeEvent(input$var, {
      if(!length(input$var)) {
        Vdata(NULL)
      } else {
        updateSelectInput(session, "sortby", choices = c("", input$var), selected = "")
        data <- data()[, c(key, input$var), with = F]
        Vdata(data)
      }
    }, ignoreNULL = F)

    observeEvent(input$sortby, {
      if(input$sortby != "") {
        data <- copy(Vdata())
        data <- setorderv(data, cols = input$sortby, na.last = T)
        Vdata(data)
      }
    })

    return(Vdata)
  })
}
