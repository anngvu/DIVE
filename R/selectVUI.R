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
#' This is a module subcomponent that originally serves under multiVCtrl but should be
#  generic enough to be integrated with other modules.
#' When data is uploaded through multiVCtrl, it can either be "high-throughput"
#' or "low-throughput" type data, the latter of which is what is commonly thought of
#' as phenotype or clinical variables. This module handles changes in the table
#' of low-throughput data. When the table adds new data columns, the selection menu changes to
#' include these new options. When the user selects specific columns from the menu,
#' the module returns the table subsetted by those columns. The most recent modification
#' involves adding parenthesized counts for each variable option, i.e. "phenotypeA (15)",
#' calculated on intersections with another table.
#'
#' @family multiVUI module functions
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param data A reactive data.table. See details.
#' @param key A key column that is kept for every selected subset. Defaults to "ID".
#' @param label Label for variable select input.
#' @param excludepattern Exclude column names with this pattern in the selection choices.
#' @param selected Optional, initial selection.
#' @param countby The matrix representing a high-throughput dataset with sample IDs, which are intersected to generate counts in the select options. See details.
#' @param maxitems Maximum number of items that can be selected. Defaults to 3.
#' @return The subsetted data table.
#' @export
selectV <- function(input, output, session,
                    data = reactive({ NULL }), key = "ID",
                    label = HTML("<strong>Phenotype/Experimental variable(s)</strong>"),
                    excludepattern = "_(SD|SE)$",
                    selected = reactive({ NULL }), countby = reactive({ NULL }),
                    maxitems = 3)  {

  Vdata <- reactiveVal(NULL)

  output$select <- renderUI({
    # shiny::req(data(), selected())
    choices <- names(data())[names(data()) != key]
    if(!is.null(excludepattern)) choices <- grep(excludepattern, choices, value = TRUE, invert = T)
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
    if(!is.null(selected())) Vdata(data()[, c(key, selected()), with = F])
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
      setorderv(data, cols = input$sortby, na.last = T)
      Vdata(data)
    }
  })

  return(Vdata)

}
