#' UI for creating a subsetted dataset
#' 
#' Shiny module UI to subset a dataset through a selection menu.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param name Optional, name for the main dataset.
#' @param label Optional, select input label.
#' @param subsets A named list of the available subset factors in the data.
#' @param hasInfo Optional, TRUE or FALSE to indicate whether to show an info/help link for module.
#' @return A \code{shiny::\link[shiny]{tagList}} for UI to subset a dataset.
#' @export
refSubsetInput <- function(id, name = id, label = "", subsets, hasInfo = F) {
  ns <- NS(id)
  tags$div(id = "refSubsetInput",
    fluidRow(
      column(1),
      column(3,
             h3(name)
      ),
      column(4,
             div(id = "refData",
                 selectInput(ns("selectSubset"), 
                             label,
                             choices = subsets, multiple = T)
             )
      ),
      column(4,
             br(),
             if(hasInfo) infoOutput(ns("requires"))
      ))
  )
}

#' Server function for creating a subsetted dataset
#' 
#' Server function for creating a reactive subsetted dataset following user interaction.
#' 
#' When multiple instances of the module are used to create subsets from different datasets
#' (as opposed to subsets from the same dataset), it might be helpful to use refkey to track
#' the origin of the subset, and especially if the subsets are to be merged on some common
#' column later.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param refData The reference data.table.
#' @param subsetfeat The name of the column containing subset factors.
#' @param subsetlist A list mapping the select options to the correct subset levels in the data.table.  
#' @param refkey Optional, a named list containing name/label for creating a key-like column,
#' where the name is the name of the column. See details for intended purpose.
#' @param infoHTML Optional, relative path to an info Rmarkdown file that can be pulled up in a modal.
#' @return A reactive subsetted data.table. 
#' @export
refSubset <- function(input, output, session,
                      refData, subsetfeat, subsetlist, refkey,
                      infoHTML) {
  
  # Help  ------------------------------------------------------- #
  modal <- callModule(info, "requires", infoHTML)
  
  # ------------------------------------------------------------- #
  
  subsetDT <- reactive({
    validate(need(input$selectSubset != "", "Please select a type subset"))
    whichSS <- unlist(subsetlist[input$selectSubset])
    SS <- refData[get(subsetfeat) %in% whichSS]
    if(!is.null(refkey)) SS[, (names(refkey)) := refkey[[1]] ]  
    SS
  })
  
  return(subsetDT)
}