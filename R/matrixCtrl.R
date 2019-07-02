#' Shiny module UI containing filter inputs for interactive matrix
#'
#' Interactive controls for a matrix (usually correlation) plot.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
matrixCtrlUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "matrixCtrlUI",
           div(class = "forceInline", numericInput(ns("minN"), HTML("mininum <i>N</i>"), min = 1, max = NA, step = 1, val = 5, width = "80px")),
           div(class = "forceInline", selectInput(ns("optgroup"), "Select rows by", choices = "", width = "150px")),
           div(class = "forceInline", selectizeInput(ns("opt"), "Rows", choices = "", multiple = T)),
           div(class = "forceInline", br(), actionButton(ns("getopt"), "", icon = icon("filter"))),
           div(class = "forceInline", br(), actionButton(ns("reset"), "", icon = icon("undo"))),
           div(class = "forceInline", uiOutput(ns("optcolselect"))),
           div(class = "forceInline", br(), actionButton(ns("print"), "Print"))
  )
}

#' Shiny module server functions to generate filter UI for interactive matrix
#'
#' Update/populate matrix filter UI options depending on available data and metadata,
#' as well as handling of user inputs to pass on to appropriate plot objects.
#'
#' What is available as filters relies on the underlying metadata.
#' See \code{\link{metadata}} for an example data object that can be passed into the parameter
#' \preformatted{metadata}. The base UI implements these filters as drop-down selections,
#' but the UI can also expand/integrate with an optional "add-on" or "augmenting" widget
#' to provide an alternate, perhaps more intuitive route of filter input.
#' For example, compared to a drop-down selection of geographical locations,
#' a map widget would provide a better selection interface. Not all types of metadata
#' can be integrated with a widget, and the module provides capability for only one widget.
#' The server function needs to return when the widget should be called (displayed).
#'
#' The module returns an object representing the original data with user-applied filters,
#' i.e. the main input to be visualized with \code{\link{interactiveMatrix}}.
#'
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param M A non-reactive data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param N A non-reactive matrix of the same dimensions as M, used as a filter layer, e.g. sample size.
#' @param cdata The non-reactive data used for generating the matrix.
#' @param metadata A data.table with "Variable" as a key column and any number of columns (metadata) to be used as filters.
#' @param newdata Optional, reactive data, e.g. from user upload, that can be merged with M.
#' @param widget Optional, associated widget input. See details.
#' @return Reactive state values to be used by the associated plotting module.
#' @export
matrixCtrl <- function(input, output, session,
                     M, N, cdata, metadata0, newdata = reactive({ }), widget = reactive({ }) ) {

  # Keep only metadata relevant to M; ideally, metadata should already match M
  metadata <- metadata[VarID %in% rownames(M)]

  default <- list(M = M, N = N, cdata = cdata, newdata = NULL, filM = M)
  state <- reactiveValues() # returned
  optrows <- reactiveVal(NULL)

  # reset all to default state
  reset <- function() {
    for(i in names(default)) state[[i]] <- default[[i]]
  }

  reset()

  #-- Filter options ---------------------------------------------------------------------------------------------------------#

  # need to return optgroup because widget visibility is tied to optgroup
  observe({
    state$optgroup <- input$optgroup
  })

  # Update optgroups based on metadata
  updateSelectInput(session, "optgroup", choices = names(metadata), selected = "VarID")

  # Update list of options when an optgroup is selected
  # Important note: It might seem strange to return options from the rownames of the current matrix
  # instead of the more obvious usage of the column in metadata,
  # but this is a workaround to the fact that the matrix can contain uploaded new data without metadata
  # (metadata is not provided with the upload).
  opt <- reactive({
    if(input$optgroup == "VarID") rownames(state$M) else metadata[[input$optgroup]]
  })

  observe({
    updateSelectizeInput(session, "opt", choices = unique(opt()),
                         options = list(placeholder = paste0("(select one or more from ", isolate(input$optgroup), ")")))
  })

  # Update options when input is given via widget
  observeEvent(widget(), {
      selected <- c(isolate(input$opt), widget())
      updateSelectizeInput(session, "opt", choices = unique(opt()), selected = selected)
  })

  #-- Filtering handling ----------------------------------------------------------------------------------------------------#
  # Return an updated matrix given inputs from two filters

  observeEvent(input$print, {
    showModal(modalDialog(verbatimTextOutput(session$ns("matrixprint"))))
  })

  output$matrixprint <- renderPrint({
    state$filM
  })

  # Reset to none selected
  observeEvent(input$reset, {
    updateSelectizeInput(session, "opt", choices = unique(opt()), selected = NULL)
    optrows(NULL)
  })

  # Updating visible parts of matrix according to selected opt
  observeEvent(optrows(), {
    if(is.null(optrows())) {
      state$filM <- state$M
    } else {
      state$filM <- filterUpdate(state$M, state$N, input$minN, optrows = optrows())
    }
  }, ignoreInit = T, ignoreNULL = F)

  # Also give column filter options
  output$optcolselect <- renderUI({
    if(!is.null(optrows())) selectizeInput(session$ns("optcol"), "Columns", "", "", multiple = T)
  })

  filterUpdate <- function(M, N, minN, optrows = rownames(M)) {
    whichopt <- which(rownames(M) %in% optrows)
    m <- M[whichopt, , drop = F]
    # exclude completely "dead" col with no values above N
    # n <- apply(N[whichopt, , drop = F], 2, max)
    # n <- which(n >= minN)
    # m <- m[, n, drop = F]
    return(m)
  }

  # Apply minimum N to current matrix filM
  observe({
    state$filM <- filterUpdate(isolate(state$M), isolate(state$N), input$minN)
  })

  # Get selected opts as VarID
  observeEvent(input$getopt, {
    if(length(input$opt)) {
      rows <- metadata$VarID[ metadata[[input$optgroup]] %in% input$opt ]
      optrows(rows)
    }
  })

  # Handle removing terms from applied
  # observe({
  #   newcurrent <- input$applied
  #   if(length(newcurrent)) {
  #     current <- isolate(applied())
  #     current <- current[current$opt %in% newcurrent, ]
  #     applied(current)
  #     updateSelectizeInput(session, "applied", choices =  newcurrent, selected = newcurrent)
  #   } else {
  #     resetApplied()
  #   }
  # })

  #-- New data handling ------------------------------------------------------------------------------------------------------#

  # Update M, cdata, and options given newdata, and reset to default when newdata is removed
  observeEvent(newdata(), {
    if(is.null(newdata())) {
      reset()
    } else {
      newDT <- newdata()
      names(newDT) <- make.names(names(newDT))
      cdata2 <- merge(cdata, newDT, by = "ID", all.x = T, all.y = F)
      updated <- suppressWarnings(data2cor(cdata2))
      state$cdata <- cdata2
      state$filM <- state$M <- updated$M
      state$N <- updated$N
      state$newdata <- names(newDT) # only the names of new variables need be stored, not full table
    }
  }, ignoreNULL = F)

  return(state)
}
