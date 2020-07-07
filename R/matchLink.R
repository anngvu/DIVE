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
                    htmlOutput(ns("matchOn")), # user feedback on current match parameters
                    htmlOutput(ns("matchN")), # user feedback on current match subsets
                    uiOutput(ns("btnRun")) # dynamically rendered when there is enough user input
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
#' Given two datasets, a "reference" and a "comparison" dataset, the module implements an interface
#' that allows users to interactively harmonize variables to be used for matching;
#' the output is used as inputs to the module \code{\link{matchResult}}:
#' \code{params$matchOpts} contains all the possible variables that could be used between the two datasets,
#' \code{params$matchOn} contains variables that are actually chosen for matching, and
#' \code{params$run} reactively transmits the "finalized" params/triggers the \code{\link{matchResult}} module.
#' The drag-and-drop interface allows the user to harmonize variables manually, e.g. the "gender" label
#' can be dropped next to the "sex" label to say that these mean the same thing. Which variables
#' the user can interact with is determined by the vars parameter.
#'
#' @section Variables List:
#' This should be a list such as \preformatted{list(Variables = c("age", "sex", "BMI", "biomarkerLevel"))} or
#' \preformatted{list(Demographic = c("age", "sex"), Clinical = c("BMI", "biomarkerLevel"))}.
#' This format allows visually grouping the variables in the display only and has no effect on matching.
#'
#' @section Guess Function:
#' A guess function can be provided to attempt guessing which variables are actually the same between the datasets,
#' which will run before the user does manual harmonization of variables. The guess function is specific
#' to the dataset and the domain. For example, it knows that "sex" is a matchable variable in the reference dataset
#' and can try to look for variables called "sex" or "gender" in the incoming dataset so that the user doesn't
#' have to specify this manually. It should also be consistent with what is specified as matchable variables,
#' so that its return (a list) should contain a list item for all vars.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refdata Reactive subsetted reference data.table.
#' @param setX Reactive data.table dataset, which typically comes from the \code{\link{newCohortInput}} module.
#' @param vars A named list of a variable set (or sets) in the data that is allowed for matching. See details.
#' @param guess Optional, name of the function to call for initial guessing of harmonized variables. See details.
#' @param informd Optional, relative path to an info Rmarkdown file that will be pulled up in a modal.
#' Recommend using for providing methods details of the matching algorithm.
#' @return Reactive list of parameter values with \code{run}, \code{matchOpts} and \code{matchOn}. See details.
#' @export
matchLink <- function(input, output, session,
                      refdata, setX,
                      vars, guess = NULL,
                      informd = system.file("help/matching_methods.Rmd", package = "DIVE")) {

  modal <- callModule(info, "help", informd)

  params <- reactiveValues(matchOpts = NULL, matchOn = NULL, run = NULL)

#-- when uploaded data X changes, re-initialize match options ------------------------------------------------------------#
  observe({
    if(!is.null(guess)) {
       matchOpts <- guess(names(setX()))
       inVars <- unlist(vars) %in% names(matchOpts)
       if(!all(inVars)) {
         for(v in unlist(vars)[!inVars]) matchOpts[v] <- list(NULL)
         warning("In matching module, does guess function need to be updated for specified matchable variables?")
       }
       params$matchOpts <- matchOpts
    } else {
       matchOpts <- list()
       for(v in unlist(vars)) matchOpts[v] <- list(NULL)
       params$matchOpts <- matchOpts
    }
  })

  # Drag-n-drop sets -----------------------------------------------------------------------------------#

  # Panel for matched/harmonized pairs
  # Either a single set or multiple sets of draggable/order-able labels
  output$varSets <- renderUI({
    varSets <- tagList()
    for(i in seq_along(vars)) {
      varSets[[i]] <- tags$div(h4(icon("link"), names(vars)[i]),
                               consortSet(vars[[i]], params$matchOpts, session$ns))
    }
    varSets
  })

  # Panel for un-matched/un-harmonized pairs
  output$varBank <- renderUI({
    extvars <- names(setX())
    used <- unlist(params$matchOpts)
    unused <- setdiff(extvars, used)
    unused <- unused[!unused %in% c("ID", "Cohort")] # Needs consideration -- some hard-coding here
    varbank <- tags$div(
      h4(icon("unlink"), "Not used"),
      consortUI(inputId = session$ns("varbank-consort"), label = NULL, items = unused,
                item_class = "un-used covariate", width = 200)
    )
    varbank
  })

  #-- Update matchOn reactive values whenever variables are moved by user --------------------------------------------#
  observe({
    cvars <- paste0("var", seq_along(params$matchOpts))
    names(cvars) <- names(params$matchOpts)
    params$matchOn <- unlist(lapply(cvars, function(v) input[[v]]))
  })

  output$matchOn <- renderPrint({
    if(length(params$matchOn)) {
      pars <- paste(names(params$matchOn), "~", params$matchOn)
      div(span(class = "firm-feedback", "You are matching on"), tags$ul(lapply(pars, tags$li)), br())
    } else {
      div(span(class = "firm-feedback", "You need to specify at least one match variable."), br())
    }
  })

  output$matchN <- renderPrint({
    if(nrow(refdata()) > 0) {
      div(span(class = "firm-feedback", "You are matching using"), br(), paste("a subset of n =", nrow(refdata())), br(), br())
    } else {
      div(span(class = "firm-feedback", "You haven't selected the reference set to use.
               You won't be able to run the match until this is specified."))
    }
  })

  output$btnRun <- renderPrint({
    if((nrow(refdata()) > 0) && length(params$matchOn)) actionButton(session$ns("run"), "Get matches")
  })

  #-- Return --------------------------------------------------------------------------------------------------#
  observe({
    # Require that there is actually refdata
    if(nrow(refdata())) params$run <- input$run
  })

  return(params)
}

#-- Helpers ----------------------------------------------------------------------------------------------#


#' Create draggable and order-able set of elements
#'
#' @param vars The subset of variables in the reference dataset used for matching.
#' @param matchOpts Optional list of guessed pairs of harmonized variables for external and reference dataset.
#' @param sessionNS Namespace function, i.e. \code{shiny::\link[shiny]{session$ns}}, important for a namespaced context.
#' @return A \code{shiny::\link[shiny]{tagList}} ui for drag-n-drop variables.
consortSet <- function(varset, matchOpts, sessionNS) {
  ix <- match(varset, names(matchOpts))
  # matchOpts value should be NULL instead of NA when no matches
  # each variable in matchOpts is initialized as (namespaced) var1, var2, var3...
  UIlist <- tagList()
  for(iv in seq_along(varset)) {
    UIlist[[iv]] <- list(consortUI(inputId = sessionNS(paste0("var", ix[iv])),
                                   label = HTML(paste(varset[iv], "&rarr;")),
                                   items = matchOpts[[ ix[iv] ]],
                                   item_class = "used covariate",
                                   classes = "one-to-one",
                                   width = 100,
                                   placeholder = "n/a"),
                                   br()) # instead of in-line display
  }
  return(UIlist)
}

