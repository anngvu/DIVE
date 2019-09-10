#' Shiny module UI containing filter inputs for interactive matrix
#'
#' Interactive controls for a matrix (usually correlation) plot.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
matrixCtrlUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "matrixCtrlUI",
           div(class = "forceInline", numericInput(ns("minN"), HTML("mininum <i>N</i>"), min = 2, max = NA, step = 1, val = 7, width = "70px")),
           div(class = "forceInline",
             div(class = "forceInline", selectInput(ns("optrowgroup"), "Select rows by", choices = "", width = "120px")),
             div(class = "forceInline", selectizeInput(ns("optrow"), "Rows", choices = "", multiple = T, width = "360px"))
           ),
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
#' @param M A non-reactive data matrix, e.g. a correlation matrix, which must have row names.
#' @param N A non-reactive matrix of the same dimensions as M, used as a filter layer, e.g. sample size.
#' @param cdata The non-reactive data used for generating the matrix.
#' @param metadata Optional, a data.table with different types of metadata/annotation to be used as filters.
#' If \code{NULL}, the only filter option are the row names/index in M.
#' @param varkey The column in metadata that maps to row names in M.
#' @param newdata Optional, reactive data, e.g. from user upload, that can be merged with M.
#' @param widget Optional, associated widget input. See details.
#' @return Reactive state values to be used by the associated plotting module.
#' @export
matrixCtrl <- function(input, output, session,
                     M, N, cdata, metadata = NULL, varkey = "VarID", newdata = reactive({ }), widget = reactive({ }) ) {

  # Make sure only what exists in M (relevant) is shown
  metadata <- metadata[get(varkey) %in% rownames(M), ]

  default <- list(M = M, N = N, cdata = cdata, newdata = NULL, rowmeta = NULL, colmeta = NULL, filM = M)
  state <- reactiveValues() # returned
  optcol <- reactiveVal(NULL)

  # Update optrowgroups based on metadata
  if(!is.null(metadata)) updateSelectInput(session, "optrowgroup", choices = names(metadata), selected = varkey)

  #-- Reset options --------------------------------------------------------------------------------------------------------#
  # reset all to default state
  reset <- function() {
    for(i in names(default)) state[[i]] <- default[[i]]
  }

  # Call reset upon startup
  reset()

  # clears filters
  clear <- function() {
    updateSelectizeInput(session, "optrow", selected = character(0))
    # updateNumericInput(session, "minN", value = 7)
    state$rowmeta <- NULL
    state$colmeta <- NULL
    optcol(NULL)
  }

  # Reset to none selected
  observeEvent(input$clear, {
    clear()
  })

  #-- Filter options ---------------------------------------------------------------------------------------------------------#

  #---- Rows

  # Update metadata choices when an optrowgroup is selected
  # Important note: It might seem strange to return options from the rownames of the current matrix
  # instead of the actual column in metadata, but this is to handle two cases:
  # 1) when metadata is not necessary and
  # 2) when matrix contains uploaded new data without metadata
  optrow <- reactive({
    if(input$optrowgroup == varkey) rownames(state$M) else unique(metadata[[input$optrowgroup]])
  })

  observeEvent(optrow(), {
    updateSelectizeInput(session, "optrow", choices = optrow(), options = list(placeholder = paste0("match ", input$optrowgroup, "...")))
  })

  # Translate opt(rows) to VarID "in the background"
  in.optrow <- reactive({
    if(length(input$optrow)) {
      if(input$optrowgroup == varkey) {
        return(input$optrow)
      } else {
        return(metadata[[varkey]][ metadata[[input$optrowgroup]] %in% input$optrow ])
      }
    }
  })

  # Updating visible parts of matrix according to selected row opt
  observeEvent(in.optrow(), {
    if(!length(in.optrow())) {
      state$filM <- state$M # same as resetting
    } else {
      state$filM <- filterUpdate(state$M, state$N, input$minN, optrows = in.optrow())
      optcol(colnames(state$filM)) #! col choices depend on current rows selected
    }
  }, ignoreInit = T, ignoreNULL = F)

  # ---- Columns

  #  Translate opt(cols), which is VarID, to metadata type specified in input$optcolgroup
  optcolx <- reactive({
    req(input$optcolgroup)
    if(input$optcolgroup == varkey) optcol() else unique(metadata[[input$optcolgroup]][metadata[[varkey]] %in% optcol()])
  })

  observeEvent(optcolx(), {
    updateSelectizeInput(session, "optcol", choices = optcolx())
  }, ignoreInit = T)


  # Render column filter options when row opts are selected
  output$optcolselect <- renderUI({
    if(length(in.optrow())) {
      div(class = "forceInline",
          div(class = "forceInline", br(), actionButton(session$ns("clear"), "Clear")),
          div(class = "forceInline", selectInput(session$ns("optcolgroup"), "Select columns by", names(metadata), selected = varkey, width = "120px")),
          div(class = "forceInline", selectizeInput(session$ns("optcol"), "Columns", choices = optcol(), selected = NULL, multiple = T, width = "360px"))
      )
    }
  })

  # Translate opt(cols) to VarID "in the background"
  in.optcol <- eventReactive(input$optcol, {
    if(length(input$optcol)) {
      if(input$optcolgroup == varkey) {
        input$optcol
      } else {
        metadata[[varkey]][ metadata[[varkey]] %in% optcol() & metadata[[input$optcolgroup]] %in% input$optcol ]
      }
    }
  }, ignoreNULL = F)

  # Updating matrix according to selected col opt
  observeEvent(in.optcol(), {
    optcols <- if(length(in.optcol())) in.optcol() else optcol()
    state$filM <- filterUpdate(state$M, state$N, input$minN, optrows = in.optrow(), optcols = optcols)
  }, ignoreNULL = F)

  #-- Filter handling ----------------------------------------------------------------------------------------------------#

  # Return a filtered matrix (filM)
  filterUpdate <- function(M, N, minN, optrows, optcols = colnames(M)) {
    M[N < minN] <- NA # gray out entries in M that does not meet min N
    whichoptrows <- which(rownames(M) %in% optrows)
    whichoptcols <- which(colnames(M) %in% optcols)
    m <- M[whichoptrows, whichoptcols, drop = F]
    # exclude completely "dead" col with no values above minN
    dead <- apply(m, 2, function(x) all(is.na(x)))
    m <- m[, !dead, drop = F]
    return(m)
  }

  # Apply minimum N to current matrix filM
  observeEvent(input$minN, {
    optrows <- if(length(in.optrow())) in.optrow() else rownames(state$M)
    optcols <- if(length(in.optcol())) in.optcol() else colnames(state$filM)
    state$filM <- filterUpdate(state$M, state$N, input$minN, optrows = optrows, optcols = optcols)
  })

  # Update row/col meta whenever filM changes
  observe({
    rowmeta <- metadata[[input$optrowgroup]][metadata[[varkey]] %in% rownames(state$filM)]
    rowmeta <- factor(rowmeta, levels = unique(metadata[[input$optrowgroup]]))
    state$rowmeta <- rowmeta

    optcolgroup <- if(is.null(input$optcolgroup)) input$optrowgroup else input$optcolgroup
    colmeta <- metadata[[optcolgroup]][metadata[[varkey]] %in% colnames(state$filM)]
    levels <- if(optcolgroup == input$optrowgroup) unique(metadata[[input$optrowgroup]]) else unique(metadata[[optcolgroup]])
    colmeta <- factor(colmeta, levels = levels)
    state$colmeta <- colmeta
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
      updateSelectInput(session, "optrowgroup", selected = varkey)

    }
  }, ignoreNULL = F)

  # -- Misc widget -----------------------------------------------------------------------------------------------------------#

  # need to return optgroup because widget visibility is tied to optgroup
  observe({
    state$optrowgroup <- input$optrowgroup
    state$optcolgroup <- if(!is.null(input$optcolgroup)) input$optcolgroup else input$optrowgroup
  })

  # Update options when input is given via widget
  observeEvent(widget(), {
    selected <- c(isolate(input$optrow), widget())
    updateSelectizeInput(session, "optrow", choices = optrow(), selected = selected)
  })

  return(state)
}
