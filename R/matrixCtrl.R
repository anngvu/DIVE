#' Shiny module UI containing filter inputs for interactive matrix
#'
#' Interactive controls for a matrix (usually correlation) plot.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
matrixCtrlUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "matrixCtrlUI",
    fluidRow(
      column(12,
             div(class = "forceInline", numericInput(ns("minN"), HTML("mininum <i>N</i>"), min = 1, max = NA, step = 1, val = 5, width = "80px")),
             div(class = "forceInline", selectInput(ns("optgroup"), "Filter by", choices = "", width = "175px")),
             div(class = "forceInline", selectizeInput(ns("opt"), icon("search"), choices = "",
                                                       multiple = T, width = "300px")),
             div(class = "forceInline", br(), actionButton(ns("addopt"), "", icon = icon("plus"))),
             div(class = "forceInline", selectizeInput(ns("applied"), "Applied tags", choices = "",
                                                       multiple = T, options = list(placeholder = "none"), width = "300px")),
             div(class = "forceInline", br(), actionButton(ns("reset"), "", icon = icon("times")))
    ))
  )
}

#' Server functions to generate filter UI for interactive matrix
#'
#' Update/populate matrix filter UI options depending on available data and metadata,
#' as well as handling of user inputs to pass on appropriate plot objects.
#'
#' Since the filter UI is composed mainly of drop-down selections, one way to expand and improve it is to integrate
#' with an "add-on" or "augmenting" widget that provides an alternative and more intuitive or interesting
#' route of input for users. Foe example, in addition to a drop-down selection of geographical locations,
#' a map widget can be available to allow users to choose more visually. Because the widget may only need to appear for
#' certain metadata, the server function needs to return when the widget should be called (displayed).
#'
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param M A non-reactive data matrix, e.g. a correlation matrix, which must have variables as rownames.
#' @param N A non-reactive matrix of the same dimensions as M with data for the filterable layer, e.g. sample size.
#' @param cdata The non-reactive data used for generating the matrix.
#' @param metadata A data.table with "Variable" as a key column and any number of columns (metadata) to be used as filters.
#' @param newdata Optional, reactive data, e.g. from user upload, that can be merged with M.
#' @param widget Optional, associated widget input. See details.
#' @return Reactive state values to be used by the associated plotting module.
#' @export
matrixCtrl <- function(input, output, session,
                     M, N, cdata, metadata, newdata = reactive({ }), widget = reactive({ }) ) {

  default <- list(M = M, N = N, cdata = cdata, newdata = NULL, filM = M)
  applied <- reactiveVal(data.frame(opt = character(0), optgroup = character(0))) # stores user selection in long format
  state <- reactiveValues(applied = applied) # returned; initialized with default values above

  # clears selected filters
  reset <- function() {
    updateSelectizeInput(session, "applied", choices = "", selected = character(0))
    applied(isolate(applied())[0, ])
  }

  # need to return optgroup because widget visibility is tied to optgroup
  observe({
    state$optgroup <- input$optgroup
  })

  #-- Filter options ---------------------------------------------------------------------------------------------------------#

  # Update optgroups based on metadata
  updateSelectInput(session, "optgroup", choices = names(metadata), selected = "Variable")

  # Update list of options when an optgroup is selected
  # It might seem strange to return options from the rownames of the current matrix
  # instead of the more obvious usage of the Variable column in metadata,
  # but this is a workaround to the fact that the matrix can change with incoming new data,
  # and it's not worth it/doable to create a new metadata table
  # (because metadata is not provided with the upload).
  opt <- reactive({
    if(input$optgroup == "Variable") rownames(state$M) else metadata[[input$optgroup]]
  })

  observe({
    updateSelectizeInput(session, "opt", choices = unique(opt()), options = list(placeholder = tolower(paste0("select from ", input$optgroup, "s.."))))
  })

  # Update options when input is given via widget
  observe({
    if(!is.null(widget())) {
      selected <- c(isolate(input$opt), widget())
      updateSelectizeInput(session, "opt", choices = unique(opt()), selected = selected)
    }
  })

  #-- New data handling ------------------------------------------------------------------------------------------------------#

  # Update M, cdata, and options given newdata, and reset to default when newdata is removed
  observe({
    if(is.null(newdata())) {
      for(i in names(default)) state[[i]] <- default[[i]]
    } else {
      reset()
      newDT <- newdata()
      names(newDT) <- make.names(names(newDT))
      cdata2 <- merge(cdata, newDT, by = "ID", all.x = T, all.y = F)
      updated <- suppressWarnings(data2cor(cdata2))
      state$cdata <- cdata2
      state$filM <- state$M <- updated$M
      state$N <- updated$N
      state$newdata <- names(newDT) # only the names of new variables need be stored, not full table
    }
  })

  #-- Filtering handling ----------------------------------------------------------------------------------------------------#
  # Return an updated matrix given inputs from two filters
  filterUpdate <- function(M, N, minN, optvars) {
    has.n <- which(apply(N, 1, max) >= minN) # exclude completely "dead" rows with no values above N
    has.opt <- which(rownames(M) %in% optvars)
    keep <- intersect(has.n, has.opt)
    M[N < minN] <- NA # gray out/remove r values calculated from less than specified n
    M <- M[keep, keep]
    return(M)
  }

  # Apply minimum N to current matrix filM
  observeEvent(input$minN, {
    state$filM <- filterUpdate(state$M, state$N, input$minN, rownames(state$filM))
  })

  # Add selected opts to "applied" list and update matrix accordingly
  observeEvent(input$addopt, {
    if(!length(input$opt)) return()
    current <- applied()
    opt <- data.frame(opt = input$opt, optgroup = input$optgroup)
    current <- unique(rbind(current, opt))
    applied(current)
    # update selections
    updateSelectizeInput(session, "opt", selected = character(0))
    updateSelectizeInput(session, "applied", choices = current$opt, selected = current$opt)
  })

  # Updating visible parts of matrix according to applied
  observe({
    applied <- applied()
    if(!nrow(applied)) {
      state$filM <- state$M
    } else {
      applied <- split(applied$opt, f = applied$optgroup)
      # map metadata terms to variables; 'Variable' optgroup potentially contains user-uploaded values,
      # i.e. see workaround in opt()
      has.opt <- lapply(names(applied)[names(applied) != "Variable"],
                     function(optgroup) metadata$Variable[metadata[[optgroup]] %in% applied[[optgroup]]])
      has.opt <- Reduce(union, c(has.opt, list(applied$Variable)))
      state$filM <- filterUpdate(state$M, state$N, input$minN, has.opt)
    }
  })

  # Handle removing terms from applied
  observe({
    newcurrent <- input$applied
    if(length(newcurrent)) {
      current <- isolate(applied())
      current <- current[current$opt %in% newcurrent, ]
      applied(current)
      updateSelectizeInput(session, "applied", choices =  newcurrent, selected = newcurrent)
    } else {
      reset()
    }
  })

  # Reset to none selected
  observeEvent(input$reset, {
    reset()
  })

  return(state)
}
