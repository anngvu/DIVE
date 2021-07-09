#' Shiny module UI for matrix view
#'
#' Interactive controls of matrix view
#'
#' This includes UI elements for selection and filtering
#' of the matrix; these can be conditionally rendered.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param selection Whether to show selection input.
#' @param minN Minimum number for numeric input filter.
#' @param nfilter Whether to show the N filter input.
#' @param pfilter Whether to show the P filter input.
#' @export
matrixCtrl2UI <- function(id, minN = 4, selection = FALSE, nfilter = FALSE, pfilter = FALSE) {
  ns <- NS(id)
  tags$div(id = ns("matrixCtrlUI"), class = "matrixCtrlUI-panel",

           div(class = "ui-inline",
               shinyWidgets::radioGroupButtons(ns("layer"), "Connection type",
                                               choices = c(`Shared cases` = "N", `Association (correlation)` = "M"))
               ),
           div(class = if(selection) "ui-inline" else "ui-inline hidden",
               div(class = "ui-inline",
                     selectizeInput(ns("optrow"),
                                    label = HTML("<strong>Selected nodes</strong>"),
                                    choices = "", selected = NULL, multiple = T, width = "500px",
                                    options = list(placeholder = paste("select to view"))
                                  )
                   )

           ),
           div(class = if(selection) "ui-inline" else "ui-inline hidden",
               conditionalPanel("input.optrow != ''", ns = ns,
                 div(class = "ui-inline", br(), icon("arrow-right", class = "fa-3x")),
                 div(class = "ui-inline",
                     selectizeInput(ns("optcol"),
                                    label = HTML("<strong>Selected connected nodes</strong>"),
                                    choices = "", multiple = T, width = "400px"))
               )
           ),
           div(class = if(nfilter) "ui-inline" else "ui-inline hidden",
               conditionalPanel("input.optcol == ''", ns = ns,
                 numericInput(ns("minN"),
                              label = "mininum N",
                              value = minN, min = minN, step = 1, width = "80px")
                 )
               ),
           div(class = if(pfilter) "ui-inline" else "ui-inline hidden", br(),
               conditionalPanel("input.optcol == ''", ns = ns,
                 checkboxInput(ns("cutoffP"),
                               label = "P < 0.05 ",
                               value = FALSE, width = "80px")
                 )
               )
           )
}

#' Shiny module server functions to control matrix view
#'
#' Create a matrix view that can be visualized with appropriate plotting modules
#'
#' The module can integrate several types of interactions that control
#' the view of a matrix. It handles recalculation of the view
#' and passes on the data to be visualized with
#' \code{\link{matrixMainServer}} or \code{\link{matrixAsNetworkServer}}.
#' The base UI implements drop-down selection for basic subsetting of matrix indices
#' and threshold-type filters, and integrates with other modules providing
#' new data input or specialized selection interface. For example, for some data
#' a map widget might provide a better selection interface than a simple
#' drop-down, thus the map widget module could be implemented separately
#' and its result communicated as \code{metafilter}.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param M A data matrix, e.g. a correlation matrix, which must have row and column names.
#' @param N A matrix of the same dimensions as `M` to be used as a filter layer, e.g. sample size.
#' @param P A matrix of the same dimensions as `M` to be used as a filter layer, e.g. p-values.
#' @param cutoffP A cutoff value for `P` to be used as default for filtering.
#' @param cdata The data used for generating the matrix,
#' necessary for allowing user-uploaded data to be integrated into mutable `M`.
#' @param metafilter Optional reactive data (table) where first column contains rows to be selected
#' and metadata attributes as additional columns.
#' @param newdata Optional reactive data (table) such as a user upload passed in by
#' the \code{\link{dataUploadServer}} module or from some other component,
#' and which is suitable for merging with \preformatted{cdata} to calculate a new `M`.
#' @param filtervals Optional reactive object for storing filter values that need to be communicated to other modules.
#' @return Reactive values in \code{mstate} object that keeps track of visible matrices
#' and selected metadata and is used by the associated plotting module.
#' @export
matrixCtrl2Server <- function(id,
                              M = NULL, N = NULL, P = NULL,
                              cutoffP = 0.05,
                              cdata = NULL,
                              metafilter = reactive({}),
                              newdata = reactive({}),
                              filtervals = NULL) {

  moduleServer(id, function(input, output, session) {

    mstate <- reactiveValues(M = N, N = N, P = P,
                             layer = "M",
                             cdata = cdata,
                             newdata = NULL,
                             rowmeta = NULL, colmeta = NULL, filM = M)

    request <- reactiveValues(optrow = NULL)

    # Some applications need filter values to be passed on in addition to mstate values
    # If so, a reactive value object should be passed into filtervals params
    obs_filter <- observe({
      filtervals$N <- input$minN
      filtervals$P <- if(input$cutoffP) cutoffP else NULL
    }, suspended = T)

    if(!is.null(filtervals)) obs_filter$resume()

    #-- Initialize UI -------------------------------------------------------------------------------#

    updateSelectizeInput(session, "optrow", choices = rownames(M))

    #-- Observe whether layer requested is M or N ---------------------------------------------------#

    observeEvent(input$layer, {
      if(input$layer == "M") {
        mstate$M <- M
        mstate$layer <- "M"
      } else {
        mstate$M <- N
        mstate$layer <- "N"
      }
      mstate$filM <- mstate$M
    })

    #-- Functions -----------------------------------------------------------------------------------#

    # Return a filtered matrix (filM)
    filterUpdate <- function(M = mstate$M, N = mstate$N, P = mstate$P,
                             minN, cutoffP = NULL,
                             optrows = rownames(M), optcols = colnames(M)) {
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


    #-- Row selection -------------------------------------------------------------------------------#

    # The optrow selection controls which rows are displayed; by default, choices are all rows in M.
    # The selected choices can be updated regularly by 1) external filter modules, 2) queryString,
    # or 3) when new data is uploaded, all of which keeps selection request in request$optrow.
    observeEvent(request$optrow, {
      selected <- request$optrow
      updateSelectizeInput(session, "optrow",
                           # label = sprintf("<strong>Available %s</strong>", input$optrowgroup), # https://github.com/rstudio/shiny/issues/1140
                           selected = selected)
      request$optrow <- NULL
    }, ignoreInit = TRUE) # because requires initial updateSelectInput to populate optrowgroup


    # Updating visible matrix rows according to selected row opt
    observeEvent(input$optrow, {
      if(!length(input$optrow)) {
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP) # mstate$M
        mstate$rowmeta <- mstate$colmeta <- NULL
      } else {
        optrows <- input$optrow
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP, optrows = optrows)
        # meta is _optionally_ returned by metafilter as the second column or list element
        if(length(metafilter()) > 1) mstate$rowmeta <- metafilter()[[2]][metafilter()[[1]] %in% rownames(mstate$filM)]
      }
    }, ignoreInit = TRUE, ignoreNULL = F, priority = 10) # matrix update should run before optcol update (see below)

    #-- Column selection --------------------------------------------------------------------------#

    # Populate optcol according to current rows selected
    observeEvent(input$optrow, {
      optcols <- colnames(mstate$filM)
      updateSelectizeInput(session, "optcol", choices = optcols)
    })


    # Updating matrix according to selected col opt
    observeEvent(input$optcol, {
      if(!length(input$optcol)) {
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP,
                                    optrows = rownames(mstate$filM), optcols = colnames(mstate$M))
        mstate$colmeta <- NULL
      } else {
        optcols <- input$optcol
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = if(input$cutoffP) cutoffP,
                                    optrows = rownames(mstate$filM), optcols = optcols)
        # Metadata for cols requires looking up current col ids with db
      }
    }, ignoreNULL = F, ignoreInit = TRUE)

    #-- N filter ----------------------------------------------------------------------------------#

    # Apply minimum N
    observeEvent(input$minN, {

        optrows <- rownames(mstate$filM)
        optcols <- colnames(mstate$filM)
        mstate$filM <- filterUpdate(minN = input$minN,
                                    cutoffP = if(input$cutoffP) cutoffP,
                                    optrows = optrows, optcols = optcols)
        #if(length(optrows) > 1) mstate$rowmeta <- optrows[[2]][optrows[[1]] %in% rownames(mstate$filM)]
        #if(length(optcols) > 1) mstate$colmeta <- optcols[[2]][optcols[[1]] %in% colnames(mstate$filM)]

    })

    #-- P filter ----------------------------------------------------------------------------------#

    observeEvent(input$cutoffP, {
      if(input$cutoffP) {
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = cutoffP,
                                    optrows = rownames(mstate$filM), optcols = colnames(mstate$filM))
      } else {
        mstate$filM <- filterUpdate(minN = input$minN, cutoffP = NULL,
                                    optrows = rownames(mstate$filM), optcols = colnames(mstate$filM))
      }
    }, ignoreInit = TRUE)

    #-- Metafilter handling -----------------------------------------------------------------------#

    # If metafilter changes, put new rows in request
    # (row indices should always be in first column/first list element)
    observeEvent(metafilter(), {
      request$optrow <- metafilter()[[1]]
    }, ignoreInit = TRUE)

    #-- New data handling -------------------------------------------------------------------------#

    # Update M, cdata, and options given newdata
    # Does not ignore NULL --> reset to default when newdata is removed
    observeEvent(newdata(), {
      if(is.null(newdata())) {
        updateSelectizeInput(session, "optrow", selected = character(0))
        mstate$cdata <- cdata
        mstate$filM <- mstate$M <- M
        mstate$N <- N
        mstate$rowmeta <- mstate$colmeta <- NULL
      } else {
        tryCatch({
          withProgress(message = "adding new data", expr = {
            newDT <- newdata()
            names(newDT) <- make.names(names(newDT))
            cdata2 <- merge(cdata, newDT, by = "ID", all.x = T, all.y = F)
            updated <- suppressWarnings(data2cor(cdata2))
            mstate$cdata <- cdata2
            mstate$filM <- mstate$M <- updated$M
            mstate$N <- updated$N
            mstate$P <- updated$P
            mstate$rowmeta <- mstate$colmeta <- NULL
            request$optrow <- names(newDT) # trigger view update to focus on new uploaded data
          })
        }, error = function(e) meh(error = e) )
      }
    }, ignoreInit = T, ignoreNULL = F)


    # --------------------------------------------------------------------------------------------#

    return(mstate)
  })
}
