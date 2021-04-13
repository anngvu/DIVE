#' Filter panel UI
#'
#' A panel with a combination of filter inputs
#'
#' Components are actually rendered using the function \code{sideFilterUI}
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
filterPanelUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter"))
}

#' Filter server
#'
#' Process and return output from user interaction with corresponding ui
#'
#' Users are essentially filtering upon values in columns of \code{dt};
#' the applied combination gives output \code{dtkey} entries,
#' the calculation of which is actually delegated to the function \code{filter4j}.
#' Because it may not make sense to use all filters,
#' \code{filterconf} allows specifying which combination of filters should be active.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param dt A \code{data.table}.
#' @param dtkey Name of unique key column in \code{dt}, defaulting to "ID".
#' @param filterconf A list with names matching columns in \code{lhdata} to be used as filters
#' and elements "type" with their input widget types, "selected" for the initial selection,
#' and "conditional" with a boolean value for whether this is a filter that should initially be hidden/ignored.
#' See \code{\link{sideFilterUI}}.
filterPanelServer <- function(id,
                              dt, dtkey = "ID",
                              filterconf) {

  moduleServer(id, function(input, output, session) {

    filteroutput <- reactiveVal(NULL)

    # Each filter col except the key col will have own input component to implement filtering
    filtercols <- names(filterconf)

    # Not all filter cols have to be shown/used on init
    # switchfiltercols tracks switches that allow users to activate a filter, which may not exist depending on filterconf
    switchfiltercols <- sapply(filtercols, function(x) paste("usefilter", gsub(".", "", x, fixed = T), sep = "_"))

    # Names of active filter cols ("on" filters)
    activefiltercols <- reactive({
      active <- unlist(sapply(switchfiltercols, function(x) input[[x]]))
      if(is.null(active) || all(active == F)) return(NULL) else filtercols[active]
    })

    # Render filtercols inputs
    output$filter <- renderUI({
      uilist <- lapply(filtercols,
                       function(col) sideFilterUI(col,
                                                  dt[[col]],
                                                  filterconf[[col]]$input,
                                                  filterconf[[col]]$selected,
                                                  conditional = filterconf[[col]]$conditional,
                                                  ns = session$ns))
      tagList(uilist)
    })

    # Process filtercols inputs
    obs_filters <- observe({
      if(is.null(activefiltercols())) {
        filteroutput(NULL)
      } else {
        ids <- lapply(activefiltercols(),
                      function(col) filter4j(dt, col, input[[paste0("filter-", col)]], dtkey, filterconf[[col]]$input))
        ids <- Reduce(intersect, ids)
        filteroutput(ids)
      }
    })

    return(filteroutput)

  })
}



#' Helper function for extracting inputs created with \code{\link{sideFilterUI}}
#'
#' Get values stored in inputs created by \code{\link{sideFilterUI}}
#'
#' @param dt A \code{data.table}.
#' @param col Filter column in \code{dt}.
#' @param values The filter value(s) in `col`.
#' @param j The column of data to return for matches of `values` in `col`.
#' @param type Type of input filter; only "range" is special and requires `values` to be a 2-element vector.
filter4j <- function(dt, col, values, j, type) {
  if(type == "range") {
    dt[get(col) >= values[1] & get(col) <= values[2], get(j)]
  } else {
    if(!length(values) || class(dt[[col]]) == "list") {
      dt[get(col) %in% list(values), get(j)]
    } else {
      dt[get(col) %in% values, get(j)]
    }
  }
}
