#' Shiny module UI containing filter inputs for interactive matrix
#'
#' Interactive controls for a matrix (usually correlation) plot.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
matrixCtrlUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "matrixCtrlUI",
           div(class = "forceInline", numericInput(ns("minN"), HTML("mininum <i>N</i>"), min = 2, max = NA, step = 1, val = 2, width = "70px")),
           div(class = "forceInline", selectInput(ns("optgroup"), "Select rows by", choices = "", width = "150px")),
           div(class = "forceInline", selectizeInput(ns("opt"), "Rows", choices = "", multiple = T, width = "400px")),
           div(class = "forceInline", br(), actionButton(ns("getopt"), "Get")),
           div(class = "forceInline", br(), actionButton(ns("reset"), "Reset")),
           div(class = "forceInline", uiOutput(ns("optcolselect")))
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
                     M, N, cdata, metadata, newdata = reactive({ }), widget = reactive({ }) ) {

  # Keep only metadata relevant to M; ideally, metadata should already match M
  metadata <- metadata[VarID %in% rownames(M)]

  default <- list(M = M, N = N, cdata = cdata, newdata = NULL, filM = M, rowmeta = NULL, colmeta = NULL)
  state <- reactiveValues() # returned
  optrows <- reactiveVal(NULL)

  # Update optgroups based on metadata
  updateSelectInput(session, "optgroup", choices = names(metadata), selected = "VarID")

  # Update list of options when an optgroup is selected
  # Important note: It might seem strange to return options from the rownames of the current matrix
  # instead of the more obvious usage of the column in metadata,
  # but this is a workaround to the fact that the matrix can contain uploaded new data without metadata
  # (metadata is not provided with the upload).
  opt <- reactive({
    if(input$optgroup == "VarID") rownames(state$M) else unique(metadata[[input$optgroup]])
  })

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

  observeEvent(opt(), {
    updateSelectizeInput(session, "opt", choices = opt(), selected = optrows(),
                         options = list(placeholder = paste0("(select one or more from ", input$optgroup, ")")))
  })

  # Update options when input is given via widget
  observeEvent(widget(), {
      selected <- c(isolate(input$opt), widget())
      updateSelectizeInput(session, "opt", choices = unique(opt()), selected = selected)
  })

  #-- Filtering handling ----------------------------------------------------------------------------------------------------#
  # Return an updated matrix given inputs from two filters

  output$matrixprint <- renderPrint({
    colnames(state$filM)
  })

  # Reset to none selected
  observeEvent(input$reset, {
    updateSelectizeInput(session, "opt", choices = unique(opt()), selected = NULL)
    optrows(NULL)
    updateNumericInput(session, "minN", value = 2)
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
    if(!is.null(optrows())) selectizeInput(session$ns("optcol"), "Columns", "", "", multiple = T, width = "400px")
  })

  filterUpdate <- function(M, N, minN, optrows = rownames(M)) {
    M[N < minN] <- NA
    whichopt <- which(rownames(M) %in% optrows)
    m <- M[whichopt, , drop = F]
    # exclude completely "dead" col with no values above minN
    dead <- apply(m, 2, function(x) all(is.na(x)))
    m <- m[, !dead, drop = F]
    return(m)
  }

  # Apply minimum N to current matrix filM
  observeEvent(input$minN, {
    optrows <- if(!is.null(optrows())) optrows() else rownames(state$M)
    state$filM <- filterUpdate(state$M, state$N, input$minN, optrows = optrows)
  })

  # Get selected opts as VarID
  observeEvent(input$getopt, {
    if(length(input$opt)) {
      if(input$optgroup == "VarID") {
        rows <- input$opt
        state$rowmeta <- NULL
      } else {
        rows <- metadata$VarID[ metadata[[input$optgroup]] %in% input$opt ]
        state$rowmeta <- metadata[[input$optgroup]][ metadata[[input$optgroup]] %in% input$opt ]
        # state$colmeta <-
      }
      optrows(rows)
    }
  })

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
      # select new data for view
      optrows(state$newdata)
      updateSelectInput(session, "optgroup", selected = "VarID")
    }
  }, ignoreNULL = F)

  return(state)
}
