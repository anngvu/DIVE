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
           div(class = "ui-inline", numericInput(ns("minN"), "mininum N", val = 5, min = minN, step = 1, width = "80px")),
           div(class = "ui-inline", br(), checkboxInput(ns("cutoffP"), "P < 0.05 ", value = FALSE, width = "80px")),
           div(class = "ui-inline",
               div(class = "ui-inline", selectInput(ns("optrowgroup"), "Filter type", choices = "", width = "120px")),
               div(class = "ui-inline", selectizeInput(ns("optrow"), "Row (from)", choices = "", multiple = T, width = "400px"))
           ),
           div(class = "ui-inline", conditionalPanel("input.optrow != ''", ns = ns,
               div(class = "ui-inline", selectInput(ns("optcolgroup"), "Filter type", choices = "", width = "120px")),
               div(class = "ui-inline", selectizeInput(ns("optcol"), "Column (to)", choices = "", multiple = T, width = "400px"))
           )),
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
#' @param M A data matrix, e.g. a correlation matrix, which must have row and column names.
#' @param N A matrix of the same dimensions as M to be used as a filter layer, e.g. sample size or p-values.
#' @param cdata The data used for generating the matrix,
#' necessary for allowing user-uploaded data for mutable M.
#' @param metadata Optional, a data.table with different types of metadata/annotation to be used as filters.
#' If not given, the only filter option will be the row names in M. Column names will be the names of the filter group.
#' @param vkey The column in metadata that maps to row/col names in M, i.e. the key column such as VarID.
#' If metadata is not given, should be something like "Name" because the only selection possible is by row names.
#' @param newdata Optional, reactive data such as a user upload passed in by
#' the \code{\link{dataUpload}} module or from some other component,
#' that is suitable for merging with \preformatted{cdata} to calculate a new M.
#' @param widgetmod IGNORE. Optional, a widget extension module. See details.
#' @return Reactive values in \code{mstate} object that keeps track of visible matrices
#' and selected metadata and is used by the associated plotting module.
#' @export
matrixCtrlServer <- function(id,
                             M = NULL, N = NULL, P = NULL, cutoffP = 0.05,
                             cdata = NULL, metadata = NULL, vkey = NULL,
                             newdata = reactive({ }),
                             widgetmod = NULL, widgetopt = NULL) {

  moduleServer(id, function(input, output, session) {

    mstate <- reactiveValues(M = M, N = N, P = P, cdata = cdata, newdata = NULL, rowmeta = NULL, colmeta = NULL, filM = M)
    request <- reactiveValues(optrowgroup = NULL, optrow = NULL)

    #-- Initialize UI --------------------------------------------------------------------------------------------------#

    # Column names in metadata become optrowgroup choices ("select rows by"), with vkey used as the default
    # Metadata should not contain names that are not row/col names of M
    # If metadata is not given, the only selection is row/col names
    if(!is.null(metadata)) {
      metadata <- metadata[get(vkey) %in% rownames(M)]
      optgroups <- names(metadata)
    } else {
      optgroups <- vkey
    }

    updateSelectInput(session, "optrowgroup", choices = optgroups, selected = vkey)
    updateSelectInput(session, "optcolgroup", choices = optgroups, selected = vkey)

    #-- URL parameter strings ----------------------------------------------------------------------------------------#

    # Handles initiating a view with URL parameter string
    observe({
      query <- parseQueryString(session$clientData$url_search)
      for(nm in names(query)) {
        if(nm %in% names(metadata) || nm == vkey) {
          request$optrowgroup <- nm
          request$optrow <- query[[nm]]
        }
      }
    }, priority = 100)

    # Update selected optrowgroup when using URL parameter string; has no effect if specified optrowgroup isn't valid
    observeEvent(request$optrowgroup, {
      updateSelectInput(session, "optrowgroup", selected = request$optrowgroup)
    }, ignoreInit = TRUE)

    #-- Functions ----------------------------------------------------------------------------------------------------#

    # Return a filtered matrix (filM)
    filterUpdate <- function(M = mstate$M, N = mstate$N, P = mstate$P,
                             minN, cutoffP = NULL, optrows = rownames(M), optcols = colnames(M)) {
      m <- M[optrows, optcols, drop = F]
      n <- N[optrows, optcols, drop = F]
      m[n < minN] <- NA # gray out entries in M that does not meet minN
      deadrows <- apply(m, 1, function(x) all(is.na(x))) # hide "dead pixels" (no values > minN)
      deadcols <- apply(m, 2, function(x) all(is.na(x)))
      m <- m[!deadrows, !deadcols, drop = F]
      if(!is.null(P) && !is.null(cutoffP) && all(dim(m) > 0)) {
        p <- P[rownames(m), colnames(m)]
        m[p > cutoffP] <- NA
      }
      return(m)
    }

    # Return row or columns indexes ordered by selected metadata, or just literal row/col names if no metadata
    # *column optgroup can be "" when first initialized
    metArrange <- function(optgroup, optsel) {
      if(optgroup == vkey) return(list(optsel))
      meta <- metadata[get(optgroup) %in% optsel, c(..vkey, ..optgroup)][order(get(optgroup))]
      mlevels <- unique(metadata[[optgroup]])
      meta[[optgroup]] <- factor(meta[[optgroup]], levels = mlevels, exclude = NULL)
      return(meta)
    }

    #-- Row filter ---------------------------------------------------------------------------------------------------#

    # The selection optrow holds metadata choices for the optrowgroup that is selected
    # Note: when optrowgroup == vkey, it might seem strange to return rownames of current matrix as options
    # instead of data from actual vkey column of metadata, but this is to handle two cases:
    # 1) when metadata is not given at all, and 2) when matrix contains uploaded new data without metadata
    observeEvent(list(input$optrowgroup, mstate$newdata), {
      optrow <- `if`(input$optrowgroup == vkey, rownames(mstate$M), unique(metadata[[input$optrowgroup]]))
      selected <- if(length(mstate$newdata)) mstate$newdata else request$optrow
      updateSelectizeInput(session, "optrow", choices = optrow, selected = selected,
                           options = list(placeholder = paste("no", input$optrowgroup, "filter applied")))
      request$optrow <- NULL
    }, ignoreInit = TRUE) # because requires initial updateSelectInput to populate optrowgroup


    # Updating visible matrix rows according to selected row opt
    observeEvent(input$optrow, {
      if(!length(input$optrow)) {
        mstate$filM <- mstate$M # reset
        mstate$rowmeta <- mstate$colmeta <- NULL
        updateNumericInput(session, "minN", value = 5)
      } else {
        optrows <- metArrange(input$optrowgroup, input$optrow)
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP, optrows = optrows[[1]])
        if(length(optrows) > 1) mstate$rowmeta <- optrows[[2]][optrows[[1]] %in% rownames(mstate$filM)]
      }
    }, ignoreInit = TRUE, ignoreNULL = F, priority = 10) # matrix update should run before optcol update (see below)

    #-- Column filter ------------------------------------------------------------------------------------------------#

    # Populate child selection according to optcolgroup choice AND current rows selected
    observeEvent(list(input$optrow, input$optcolgroup), {
      optcol <- `if`(input$optcolgroup == vkey, colnames(mstate$filM),
                     unique(metadata[[input$optcolgroup]][metadata[[vkey]] %in% colnames(mstate$filM)]))
      updateSelectizeInput(session, "optcol", choices = optcol,
                           options = list(placeholder = paste("no", input$optcolgroup, "filter applied")))
    })


    # Updating matrix according to selected col opt
    observeEvent(input$optcol, {
      if(!length(input$optcol)) {
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP,
                                    optrows = rownames(mstate$filM), optcols = colnames(mstate$M))
        mstate$colmeta <- NULL
      } else {
        optcols <- metArrange(input$optcolgroup, input$optcol)
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP,
                                    optrows = rownames(mstate$filM), optcols = optcols[[1]])
        if(length(optcols) > 1) mstate$colmeta <- optcols[[2]][optcols[[1]] %in% colnames(mstate$filM)]
      }
    }, ignoreNULL = F, ignoreInit = TRUE)

    #-- N filter -----------------------------------------------------------------------------------------------------#

    # Apply minimum N to current matrix filM
    observeEvent(input$minN, {

        optrows <- if(!length(input$optrow)) list(rownames(mstate$filM)) else metArrange(input$optrowgroup, input$optrow)
        optcols <- if(!length(input$optcol)) list(colnames(mstate$filM)) else metArrange(input$optcolgroup, input$optcol)
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP,
                                    optrows = optrows[[1]], optcols = optcols[[1]])
        if(length(optrows) > 1) mstate$rowmeta <- optrows[[2]][optrows[[1]] %in% rownames(mstate$filM)]
        if(length(optcols) > 1) mstate$colmeta <- optcols[[2]][optcols[[1]] %in% colnames(mstate$filM)]

    })

    #-- P filter -----------------------------------------------------------------------------------------------------#

    observeEvent(input$cutoffP, {
      if(input$cutoffP) {
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = cutoffP,
                                    optrows = rownames(mstate$filM), optcols = colnames(mstate$filM))
      } else {
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = NULL,
                                    optrows = rownames(mstate$filM), optcols = colnames(mstate$filM))
      }
    }, ignoreInit = TRUE)

    #-- New data handling --------------------------------------------------------------------------------------------#

    # Update M, cdata, and options given newdata, and reset to default when newdata is removed
    observeEvent(newdata(), {
      if(is.null(newdata())) {
        updateSelectizeInput(session, "optrow", selected = character(0))
        mstate$cdata <- cdata
        mstate$filM <- mstate$M <- M
        mstate$N <- N
        mstate$rowmeta <- mstate$colmeta <- NULL
        mstate$newdata <- NULL
      } else {
        tryCatch({
          withProgress(message = "adding new data", style = "old", expr = {
            newDT <- newdata()
            names(newDT) <- make.names(names(newDT))
            cdata2 <- merge(cdata, newDT, by = "ID", all.x = T, all.y = F)
            updated <- suppressWarnings(data2cor(cdata2))
            mstate$cdata <- cdata2
            mstate$filM <- mstate$M <- updated$M
            mstate$N <- updated$N
            mstate$rowmeta <- mstate$colmeta <- NULL
            updateSelectInput(session, "optrowgroup", selected = vkey)
            mstate$newdata <- names(newDT) # only the names of new variables need be stored, which triggers view update
          })
        }, error = function(e) meh() )
      }
    }, ignoreInit = T, ignoreNULL = F)


    # -- Misc widget -------------------------------------------------------------------------------------------------#

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

    # -- Bookmarking ------------------------------------------------------------------------------------------------#

    onBookmark(function(state) {
      state$values$optrowgroup <- input$optrowgroup
      state$values$optrow <- input$optrow
    })

    onRestore(function(state) {
      request$optrowgroup <- state$values$optrowgroup
      request$optrow <- state$values$optrow
    })

    return(mstate)
  })
}
