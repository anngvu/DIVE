#' Shiny module UI containing filter inputs for interactive matrix
#'
#' Interactive controls for a matrix (usually correlation) plot.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param minN Minimum number for numeric input filter.
#' @export
matrixCtrlUI <- function(id, minN = 5) {
  ns <- NS(id)
  tags$div(id = "matrixCtrlUI",
           div(class = "forceInline", numericInput(ns("minN"), HTML("mininum <i>N</i>"), min = minN, max = NA, step = 1, val = 2, width = "70px")),
           div(class = "forceInline",
             div(class = "forceInline", selectInput(ns("optrowgroup"), "Select rows by", choices = "", width = "120px")),
             div(class = "forceInline", selectizeInput(ns("optrow"), "Rows", choices = "", multiple = T, width = "360px"))
           ),
           div(class = "forceInline", uiOutput(ns("optcolselect"))),
           uiOutput(ns("usewidget"))
  )
}

#' Shiny module server functions to generate filter UI for interactive matrix
#'
#' Update matrix data and metadata for the appropriate plotting module.
#'
#' What is available as filters relies on the underlying metadata.
#' The metadata should be a table that can be passed into the parameter
#' \preformatted{metadata}, with a key column referencing the matrix index
#' and additional columns for each type of metadata attribute.
#' The base UI implements interactive filters as drop-down selections.
#' The server function returns the data with user-applied filters,
#' i.e. the main input to be visualized with \code{\link{iMatrix}}.
#'
#' @section To-do:
#' The UI can also expand/integrate with an optional "add-on" widget
#' to provide an alternate, perhaps more intuitive interface for filtering.
#' For example, compared to a drop-down selection of geographical locations,
#' a map widget would provide a better selection interface. Not all types of metadata
#' can be integrated with a widget, and the module provides capability for only one widget.
#' The server function needs to return when the widget should be called (displayed).
#'
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param M A data matrix, e.g. a correlation matrix, which must have row names.
#' @param N A matrix of the same dimensions as M to be used as a filter layer, e.g. sample size or p-values.
#' @param cdata The data used for generating the matrix,
#' necessary for allowing user-uploaded data for mutable M.
#' @param metadata Optional, a data.table with different types of metadata/annotation to be used as filters.
#' If not given, the only filter option will be the row names in M. Column names will be the names of the filter group.
#' @param vkey The column in metadata that maps to row/col names in M, i.e. the key column.
#' @param newdata Optional, reactive data such as a user upload passed in by
#' the \code{\link{dataUpload}} module or from some other component,
#' that is suitable for merging with \preformatted{cdata} to calculate a new M.
#' @param widgetmod IGNORE. Optional, a widget extension module. See details.
#' @return Reactive values in \code{mstate} object that keeps track of visible matrices
#' and selected metadata and is used by the associated plotting module.
#' @export
matrixCtrl <- function(input, output, session,
                       M = NULL, N = NULL, cdata = NULL, metadata = NULL, vkey = NULL,
                       newdata = reactive({ }),
                       widgetmod = NULL, widgetopt = NULL
                     ) {

  # If for some reason the metadata contains records for features that are actually not in M,
  # make sure that is reflected in the metadata selection options.
  # Columns in metadata are mapped to optrowgroups, with vkey used as the default group
  if(!is.null(metadata)) {
    metadata <- metadata[get(vkey) %in% rownames(M), ]
    updateSelectInput(session, "optrowgroup", choices = names(metadata), selected = vkey)
  } else {
    updateSelectInput(session, "optrowgroup", choices = vkey, selected = vkey)
  }

  mstate <- reactiveValues(M = M, N = N, cdata = cdata, newdata = NULL, rowmeta = NULL, colmeta = NULL, filM = M)
  request <- reactiveValues(optrowgroup = NULL, optrow = NULL)

  # handles initiating a view with URL parameter string
  observe({
    query <- parseQueryString(session$clientData$url_search)
    for(nm in names(query)) {
      if(nm %in% names(metadata) || nm == vkey) {
        request$optrowgroup <- nm
        request$optrow <- query[[nm]]
      }
    }
  }, priority = 100)

  observeEvent(request$optrowgroup, {
    updateSelectInput(session, "optrowgroup", selected = request$optrowgroup)
  })

  #-- Functions --------------------------------------------------------------------------------------------------------#

  # clears filters
  clear <- function() {
    updateSelectizeInput(session, "optrow", selected = character(0))
    # mstate$rowmeta <- NULL
    # mstate$colmeta <- NULL
  }

  # Return a filtered matrix (filM)
  filterUpdate <- function(M, N, minN, optrows, optcols = colnames(M)) {
    M[N < minN] <- NA # gray out entries in M that does not meet minN
    whichoptrows <- which(rownames(M) %in% optrows)
    whichoptcols <- which(colnames(M) %in% optcols)
    m <- M[whichoptrows, whichoptcols, drop = F]
    # exclude completely "dead" col with no values above minN
    dead <- apply(m, 2, function(x) all(is.na(x)))
    m <- m[, !dead, drop = F]
    return(m)
  }

  # ----------------------------------------------------------------------------------------------------------------------#

  # Set row options to none selected
  observeEvent(input$clear, {
    clear()
  })

  #-- Row filter ---------------------------------------------------------------------------------------------------------#

  # optrow holds metadata choices for the optrowgroup that is selected
  # important note: when optrowgroup is the vkey,
  # it might seem strange to return options from the rownames of the current matrix
  # instead of the actual column in the metadata table, but this is to handle two cases:
  # 1) when metadata is not given at all
  # 2) when matrix contains uploaded new data without metadata
  optrow <- reactive({
    if(input$optrowgroup == vkey) rownames(mstate$M) else unique(metadata[[input$optrowgroup]])
  })

  # When there are new optrow options, populate input$optrow with these options
  observeEvent(optrow(), {
    updateSelectizeInput(session, "optrow", choices = optrow(), selected = request$optrow,
                         options = list(placeholder = paste0("match ", input$optrowgroup, "...")))
    request$optrow <- NULL
  })

  # Translate opt(rows) to vkey in the background
  in.optrow <- eventReactive(input$optrow, {
    if(length(input$optrow)) {
      if(input$optrowgroup == vkey) {
        input$optrow
      } else {
        metadata[[vkey]][metadata[[input$optrowgroup]] %in% input$optrow]
      }
    }
  }, ignoreNULL = F)

  # Updating visible parts of matrix according to selected row opt
  observeEvent(in.optrow(), {
    if(!length(in.optrow())) {
      mstate$filM <- mstate$M # same as resetting
    } else {
      mstate$filM <- filterUpdate(mstate$M, mstate$N, input$minN, optrows = in.optrow())
    }
  }, ignoreNULL = F)

  #-- Column filter ---------------------------------------------------------------------------------------------------------#

  observeEvent(input$optcolgroup, {
    choices <- if(input$optcolgroup == vkey) {
        colnames(mstate$filM)
      } else {
        unique(metadata[[input$optcolgroup]][metadata[[vkey]] %in% colnames(mstate$filM)])
      }
    updateSelectizeInput(session, "optcol", choices = choices)
  })

  # Render column filter options when row opts are selected
  output$optcolselect <- renderUI({
    if(length(in.optrow())) {
      # selected input$optcolgroup will match input$optrowgroup by default
      if(isolate(input$optrowgroup) == vkey) {
        choices <- colnames(isolate(mstate$filM))
        div(class = "forceInline",
            div(class = "forceInline", br(), actionButton(session$ns("clear"), "Clear")),
            div(class = "forceInline", selectInput(session$ns("optcolgroup"), "Select columns by", vkey, selected = vkey, width = "120px")),
            div(class = "forceInline", selectizeInput(session$ns("optcol"), "Columns", choices = choices, selected = NULL, multiple = T, width = "360px"))
        )
      } else {
        choices <- unique(metadata[[isolate(input$optrowgroup)]][metadata[[vkey]] %in% colnames(isolate(mstate$filM))])
        div(class = "forceInline",
            div(class = "forceInline", br(), actionButton(session$ns("clear"), "Clear")),
            div(class = "forceInline", selectInput(session$ns("optcolgroup"), "Select columns by", names(metadata), selected = isolate(input$optrowgroup), width = "120px")),
            div(class = "forceInline", selectizeInput(session$ns("optcol"), "Columns", choices = choices, selected = NULL, multiple = T, width = "360px"))
        )
      }
    }
  })

  # Translate opt(cols) to vkey in the background
  in.optcol <- eventReactive(input$optcol, {
    if(length(input$optcol)) {
      if(input$optcolgroup == vkey) {
        input$optcol
      } else {
        metadata[[vkey]][ metadata[[input$optcolgroup]] %in% input$optcol ]
      }
    }
  }, ignoreNULL = F)

  # Updating matrix according to selected col opt
  observeEvent(in.optcol(), {
    optcols <- if(length(in.optcol())) in.optcol() else colnames(mstate$M)
    mstate$filM <- filterUpdate(mstate$M, mstate$N, input$minN, optrows = rownames(mstate$filM), optcols = optcols)
  }, ignoreNULL = F)

  #-- N filter  ---------------------------------------------------------------------------------------------------------#

  # Apply minimum N to current matrix filM
  observeEvent(input$minN, {
    optrows <- if(length(in.optrow())) in.optrow() else rownames(mstate$filM)
    optcols <- if(length(in.optcol())) in.optcol() else colnames(mstate$filM)
    mstate$filM <- filterUpdate(mstate$M, mstate$N, input$minN, optrows = optrows, optcols = optcols)
  })

  # Update group metadata labels whenever optrowgroup changes
  observe({
    if(!is.null(metadata)) {
      rowmeta <- metadata[[input$optrowgroup]][metadata[[vkey]] %in% rownames(mstate$filM)]
      rlevels <- sample(unique(metadata[[input$optrowgroup]])) # to make sure group annotation colors are different
      rowmeta <- factor(rowmeta, levels = rlevels)
      mstate$rowmeta <- rowmeta

      optcolgroup <- if(is.null(input$optcolgroup)) input$optrowgroup else input$optcolgroup
      colmeta <- metadata[[optcolgroup]][metadata[[vkey]] %in% colnames(mstate$filM)]
      # when optcolgroup and optrowgroup are the same, harmonize levels used
      levels <- if(optcolgroup == input$optrowgroup) rlevels else unique(metadata[[optcolgroup]])
      colmeta <- factor(colmeta, levels = levels)
      mstate$colmeta <- colmeta
    }
  })

  #-- New data handling ------------------------------------------------------------------------------------------------------#

  # Update M, cdata, and options given newdata, and reset to default when newdata is removed
  observeEvent(newdata(), {
    if(is.null(newdata())) {
      clear()
      mstate$cdata <- cdata
      mstate$filM <- mstate$M <- M
      mstate$N <- N
      mstate$newdata <- NULL
    } else {
      newDT <- newdata()
      names(newDT) <- make.names(names(newDT))
      cdata2 <- merge(cdata, newDT, by = "ID", all.x = T, all.y = F)
      updated <- suppressWarnings(data2cor(cdata2))
      mstate$cdata <- cdata2
      mstate$filM <- mstate$M <- updated$M
      mstate$N <- updated$N
      mstate$newdata <- names(newDT) # only the names of new variables need be stored, not full table
      updateSelectInput(session, "optrowgroup", selected = vkey) # select new data for view
    }
  }, ignoreInit = T, ignoreNULL = F)

  # -- Misc widget -----------------------------------------------------------------------------------------------------------#

  # Show widget when widget-associated optrowgroup is selected
  # output$usewidget <- renderUI({
  #   if(input$optrowgroup == widgetopt) {
  #     absolutePanel(id = "cellpackpanel", draggable = T, left = 300,
  #                   cellPackUI(session$ns("widget")))
  #   }
  # })
  #
  # if(!is.null(widgetmod)) {
  #   callModule(widgetmod, "widget")
  # }

  # Update options when input is given via widget
  # observeEvent(widget(), {
  #   selected <- c(isolate(input$optrow), widget())
  #   updateSelectizeInput(session, "optrow", choices = optrow(), selected = selected)
  # })

  # -- Bookmarking ----------------------------------------------------------------------------------------------------------#

  onBookmark(function(state) {
    state$values$optrowgroup <- input$optrowgroup
    state$values$optrow <- input$optrow
  })

  onRestore(function(state) {
    request$optrowgroup <- state$values$optrowgroup
    request$optrow <- state$values$optrow
  })

  return(mstate)
}
