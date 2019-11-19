#' Shiny module UI for interactive drag-and-drop variable harmonization of two datasets
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @return Interactive drag-and-drop UI elements.
#' @export
matchLinkUI <- function(id) {
  ns <- NS(id)
  tags$div(id = "matchLinkUI",
           fluidRow(
             column(3,
                    infoOutput(ns("help"), label = "Method details", i = "question-circle"),
                    br(), br(),
                    HTML("<strong>You are matching on</strong>"),
                    verbatimTextOutput(ns("matchOn")),
                    HTML("<strong>You are matching with</strong>"),
                    verbatimTextOutput(ns("matchN")),
                    actionButton(ns("run"), "Get matches")
             ),
             column(1),
             column(8,
                    fluidRow(div(id = "matchVars",
                      column(6, div(id = "matchVarBank",
                             uiOutput(ns("varBank")))
                      ),
                      column(6, div(id = "matchVarLinked",
                             uiOutput(ns("varSets")))
                      )
                    ))
            )
  ))
}

#' Server module function for interactive drag-and-drop variable harmonization of two datasets
#'
#' Given two datasets, a "reference" and a new "comparison" dataset, the module implements an interface
#' through which a user can create a harmonized dataset with variables in common. If provided, a guess function
#' attempts to guess which variables are actually the same, e.g. something called
#' "sex" in one dataset is the same as "gender" in another. The guess function \code{\link{guessFun}}
#' that comes with the package is a naive implementation and limited to guessing at
#' variables typical in clinical cohort datasets (e.g. age, sex, BMI, race, hba1c)
#' and should certainly be swapped out when the module is expected to handle a different variety of data.
#' The drag-and-drop interface allows the user to harmonize variables manually, e.g. the "gender" label
#' can be dropped next to the "sex" label to say that these mean the same thing. The outputs of this module
#' are inputs to \code{\link{matchResult}}:
#' \code{params$matchOpts} contains all the possible variables that could be used between the two datasets,
#' \code{params$matchOn} contains variables that are actually chosen for matching, and
#' \code{params$run} reactively transmits the "finalized" params/triggers the \code{\link{matchResult}} module.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refData Reactive subsetted reference data.table.
#' @param setX Reactive data.table dataset, which typically comes from the \code{\link{newCohortInput}} module.
#' @param vars Optional, a named list of a variable set (or sets) allowed for matching.
#' If not provided, the first 10 variables in the reference cohort dataset is used.
#' @param guess Optional, name of the function to call for initial guessing of harmonized variables;
#' no initial guesses made if no argument is provided or function does not exist.
#' @param informd Optional, relative path to an info Rmarkdown file that will be pulled up in a modal.
#' Recommend using for providing methods details of the matching algorithm.
#' @return Reactive list of parameter values with \code{run}, \code{matchOpts} and \code{matchOn}. See details.
#' @export
matchLink <- function(input, output, session,
                      refData, setX, vars, guess, informd = system.file("help/matching_methods.Rmd", package = "DIVE")) {

  modal <- callModule(info, "help", informd)

  params <- reactiveValues(matchOpts = NULL, matchOn = NULL, run = NULL)

#-- when comparison set X changes, re-initialize match options ------------------------------------------------------------#
  observe({
    if(!is.null(vars) & !is.null(guess)) {
      params$matchOpts <- guess(names(setX()))
    } else if(!is.null(guess)) { # if variables used for matching are not specified, use the first 10
      default <- list(Variables = head(names(refData()), 10))
      params$matchOpts <- guess(names(setX()))
    } else {
      params$matchOpts <- "" # TO DO! Check-later
    }
  })

  # Drag-n-drop sets -----------------------------------------------------------------------------------#

  # Panel for matched/harmonized pairs
  output$varSets <- renderUI({
    varSets <- tagList()
    for(i in seq_along(vars)) {
      varSets[[i]] <- tags$div(h4(icon("link"), names(vars)[i]),
                               orderSet(vars[[i]], params$matchOpts, session$ns)) # a set of draggable/order-able labels
    }
    varSets
  })

  # Panel for un-matched/un-harmonized pairs
  output$varBank <- renderUI({
    extvars <- names(setX())
    used <- params$matchOpts
    unused <- setdiff(extvars, used)
    unused <- unused[unused != "ID"] # Check-later: "ID" column hard-coded here.
    tags$div(
      h4(icon("unlink"), "Not used"),
      newOrderInput(inputId = session$ns("varBank"), label = NULL, items = unused,
                    connect = session$ns(paste0("var", seq_along(used))),
                    item_class = "covariate unused", width = 200)
    )
  })

  #-- Update matchOn reactive value whenever variables are moved by user --------------------------------------------#
  observe({
    cvars <- paste0("var", seq_along(params$matchOpts), "_order")
    names(cvars) <- names(params$matchOpts)
    params$matchOn <- unlist(lapply(cvars, function(v) input[[v]]))
  })

  output$matchOn <- renderPrint({
    params$matchOn
  })

  output$matchN <- renderPrint({
    cat(paste("a subset of", nrow(refData())))
  })

  #-- Return --------------------------------------------------------------------------------------------------#
  observe({
    if(!is.null(refData())) params$run <- input$run
  })

  return(params)
}

#-- Helper functions ------------------------------------------------------------------------------------------#

#' Custom function for trying to harmonize diabetes-relevant variables in two different  datasets
#'
#' @param vars Vector of variable names from an external (cohort) dataset.
#' An attempt will be made to harmonize them to variable names in the reference dataset.
#' @return List of named list elements, containing only those reference variables that seem to have
#' a corresponding version in the external variables (list elements).
#' @export
guessMatch <- function(extvars) {
  fuzzymap <- list(GADA.pos = "gad", IA2A.pos = "ia2", mIAA.pos = "mIA", ZnT8A.pos = "znt8",
             AutoAb.count = "abcount", age = "age", BMI = "BMI", db.duration = "duration",
             age.onset = "onset", Cpeptide = "cpeptide", HbA1c = "hba1c", peak.gluc = "gluc",
             race_AfricanAmerican = "afr", race_AmericanIndian = "ind", race_Asian = "asian",
             race_Caucasian = "cau", race_Hispanic.Latino = "hisp", race_Multiracial = "multiracial",
             sex_Female = "female", sex_Male = "[^fe]male")
  # Basically, find the first variable that seems close enough to each of the variables in reference
  res <- lapply(fuzzymap, function(f) sort(grep(f, extvars, val = T, ignore.case = T))[1])
  res[sapply(res, is.na)] <- list(NULL) # Remove any terms without matches from result list
  return(res)
}

#' Create draggable and order-able set of elements
#'
#' @param vars The subset of variables in the reference dataset used for matching.
#' @param matchOpts Match options, i.e. guessed pairs of matching variables for external and reference dataset.
#' @return A \code{shiny::\link[shiny]{tagList}} of variables that can be dragged and dropped.
#' @export
orderSet <- function(vars, matchOpts, sessionNS) {
  ix <- match(vars, names(matchOpts))
  UIlist <- tagList()
  for(v in seq_along(vars)) {
    UIlist[[v]] <- list(newOrderInput(inputId = sessionNS(paste0("var", ix[v])),
                                      label = HTML(paste(vars[v], "&rarr;")),
                                      items = matchOpts[[ ix[v] ]],
                                      connect = c(sessionNS(paste0("var", seq_along(matchOpts))), sessionNS("varBank")),
                                      item_class = "covariate used",
                                      width = 200,
                                      placeholder = "n/a"),
                                      br()) # force each pair on its own line
  }
  return(UIlist)
}

#' Modified version of \code{shinyjqui::\link[shinyjqui]{orderInput}};
#' important changes include \code{item_class} not being constrained and allowed to be assigned to a
#' a custom class with a forced break.
newOrderInput <- function(inputId, label, items,
                       as_source = FALSE, connect = NULL,
                       item_class,
                       placeholder = NULL,
                       width = "500px", ...) {
  if (is.null(connect)) {
    connect <- "false"
  } else {
    connect <- paste0("#", connect, collapse = ", ")
  }
  # item_class <- sprintf("btn btn-%s", match.arg(item_class))

  if (length(items) == 0 || (!is.vector(items) && !is.factor(items))) {
    item_tags <- list()
  } else {
    if (is.vector(items)) {
      item_values <- unlist(items, recursive = FALSE, use.names = TRUE)
      nms <- names(item_values)
      item_html <- `if`(
        is.null(nms) || any(nms == "") || any(is.na(nms)),
        item_values, nms
      )
    } else if (is.factor(items)) {
      item_values <- as.numeric(items)
      item_html <- as.character(items)
    }
    item_tags <- lapply(1:length(item_values), function(i) {
      tag <- shiny::tags$div(
        item_html[i],
        `data-value` = item_values[i],
        class = item_class, style = "margin: 1px"
      )
      if (as_source) {
        options <- list(connectToSortable = connect, helper = "clone", cancel = "")
        tag <- shinyjqui::jqui_draggable(tag, options = options)
      }
      return(tag)
    })
  }

  style <- sprintf(
    "width: %s; font-size: 15px; min-height: 25px;",
    shiny::validateCssUnit(width)
  )
  container <- shiny::tagSetChildren(
    shiny::tags$div(id = inputId, style = style, ...),
    list = item_tags
  )
  if (!as_source) {
    cb <- "function(e, ui){if(!$(e.target).children().length)$(e.target).empty();}"
    func <- 'function(event, ui){
      return $(event.target).children().map(function(i, e){
        return $(e).attr("data-value");
      }).get();
    }'
    options <- list(
      connectWith = connect,
      remove = htmlwidgets::JS(cb),
      shiny = list(
        order = list(
          sortcreate = htmlwidgets::JS(func),
          sortupdate = htmlwidgets::JS(func)
        )
      )
    )
    container <- shinyjqui::jqui_sortable(container, options = options)
  }

  if (!is.null(placeholder)) {
    css <- '#%s:empty:before{content: "%s"; font-size: 14px; opacity: 0.5;}'
    placeholder <- shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            sprintf(css, inputId, placeholder)
          )
        )
      )
    )
  }

  shiny::tagList(
    placeholder,
    shiny::tags$label(label, `for` = inputId),
    container
  )
}
