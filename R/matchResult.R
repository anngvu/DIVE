#' Shiny module UI output for match results
#'
#' Shiny module UI output for match results
#'
#' Creates UI for match result output, including a table preview of the result and download interface.
#' Note that an intermediate result table is available to download only when application is run in dev mode.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return Results UI consisting of a table and data download interface.
#' @export
matchResultOutput <- function(id) {
  ns <- NS(id)
  tags$div(id = "matchResultOutput",
           tableOutput(ns("table")),
           br(),
           downloadButton(ns("save"), "Save result table"),
           if(dev_mode()) downloadButton(ns("save_intermediate"), "Save result intermediates") else NULL
  )
}

#' Shiny module server for generating match results output
#'
#' Carries out matching and returns a match result reactive object
#'
#' The module initializes a \code{results} reactive values list containing \code{NULL} values and
#' whenever new \code{inputdata} is passed in (clearing previous results).
#' The matching doesn't run until a valid \code{params} reactive object is provided,
#' which will call upon the underlying handler \code{matchPair}.
#' If successful, the module updates and returns the \code{results} reactive values list object
#' with \code{results$params} \code{results$intermediate} \code{results$pair} \code{results$matchtable}.
#' Though \code{results$matchtable} is rendered in this module's UI with its own download handler,
#' it and the rest are relevant data necessarily forwarded to \code{\link{matchPlotServer}}.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param refdata Reactive refdata dataset as a \code{data.table}.
#' @param inputdata Reactive \code{data.table} of the comparison dataset, e.g from \code{\link{newDatasetServer}}.
#' @param params Reactive parameters data, e.g. from \code{\link{matchLinkServer}}.
#' @param sourcecol Name for the source key column of the joined dataset.
#' @return Reactive values containing params, intermediate results, pair, and matchtable.
#' @export
matchResultServer <- function(id,
                              refdata, inputdata, params, sourcecol) {

  moduleServer(id, function(input, output, session) {

    results <- reactiveValues(params = NULL, intermediate = NULL, pair = NULL, matchtable = NULL)

    observeEvent(inputdata(), {
      results$params <- results$intermediate <- results$pair <- results$matchtable <- NULL
    })

    observeEvent(params$run, {

      withProgress({
        tryCatch({
          # Create fused dataset with required structure, then do match
          intermediate <- dataFusion(d1 = refdata(), d2 = inputdata(),
                                     fuseon = params$matchOn, sourcecol = sourcecol)
          matchpairs <- matchPair(data = intermediate, groupcol = sourcecol, params$matchOn)
          # Update result reactive vals
          results$params <- params$matchOn
          results$intermediate <- intermediate
          results$pair <- matchpairs$pair
          results$matchtable <- matchpairs$matchtable
        }, error = function(e) meh()
      )}, value = 0.5, message = "Getting matches...")
    })

    output$table <- renderTable({
      req(results$matchtable)
      results$matchtable
    }, striped = T)

    output$save <- downloadHandler(
      filename = function() {
        "match_result.csv"
      },
      content = function(file) {
        write.csv(results$matchtable, file, row.names = F)
      }
    )

    # The intermediate table is the fused dataset in long format from dataFusion
    output$save_intermediate <- downloadHandler(
      filename = function() {
        "match_intermediate.csv"
      },
      content = function(file) {
        write.csv(results$intermediate, file, row.names = F)
      }
    )

    return(results)
  })

}

#-- Helper functions -----------------------------------------------------------------------------------#

#' Fuse two datasets based on harmonized variable names
#'
#' Concatenate two datasets given information on which columns are the same,
#' removing NA values and keeping only the specified columns in the result
#'
#' @param d1 A data.frame of the first dataset.
#' @param d2 A data.frame of the second dataset.
#' @param fuseon A named vector of the harmonized features,
#' where the names are the features in d1 and elements are features in d2.
#' @param sourcecol The key column used to identify the row sources after the two datasets are fused.
#' @return A data.table of the fused data.
#' @export
dataFusion <- function(d1, d2, fuseon, sourcecol) {
  d1 <- data.table::as.data.table(d1)
  d2 <- data.table::as.data.table(d2)
  data.table::setnames(d2, old = fuseon, new = names(fuseon))
  fused <- rbind(d1, d2, use.names = T, fill = T)
  # remove NAs
  fused <- fused[ fused[, !Reduce(`|`, lapply(.SD, function(x) is.na(x))), .SDcols = names(fuseon)] ]
  fused <- fused[, c("ID", sourcecol, names(fuseon)), with = F]
  # sourecol must be factored for use in matching function
  fused[[sourcecol]] <- factor(fused[[sourcecol]], levels = c(unique(d1[[sourcecol]]), unique(d2[[sourcecol]])))
  fused
}

#' Matching main function
#'
#' Calls \code{\link[optmatch]{pairmatch}} to perform matching and formats results
#'
#' @param data The data.
#' @param groupcol Name of the column containing groups to match between.
#' @param matchon Features to match on.
#' @return A list containing result and a table where each row corresponds to a matched pair:
#' \code{list(pair = pair, matchtable = matchtable)}
#' @export
matchPair <- function(data, groupcol, matchon) {
  dataset <- as.data.frame(data)
  dataset[[groupcol]] <- as.integer(dataset[[groupcol]]) - 1
  matchformula <- as.formula(paste(groupcol, "~", paste(names(matchon), collapse = " + ")))
  result <- optmatch::pairmatch(matchformula, data = dataset)
  pair <- result
  result <- na.omit(result)
  result <- split(as.numeric(names(result)), f = result)
  index1 <- sapply(result, `[[`, 1) # ref
  index2 <- sapply(result, `[[`, 2)  # ext
  matchtable <- setNames(data[index2, c(groupcol, "ID", names(matchon)), with = F],
                       paste0("match.", c(groupcol, "ID", names(matchon))))
  matchtable <- cbind(data[index1, c(groupcol, "ID", names(matchon)), with = F], matchtable)
  return(list(pair = pair, matchtable = matchtable))
}
