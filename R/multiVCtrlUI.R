#' Shiny module UI for controlling multi-column view
#'
#' Shiny module UI for controlling multi-column view
#'
#' Multiple high dimensional datasets can be displayed by calling separate instances of \code{\link{multiVUI}} modules,
#' with the contents typically laid out in separate a row containers. This "controller" UI establishes three different ways to
#' select and source the datasets:
#' \enumerate{
#'   \item A selection menu that contains datasets that are pre-processed and stored in memory.
#'   \item An upload method for users to upload their own datasets.
#'   \item A beta (and least-supported) method for retrieving datasets from GEO.
#' }
#' For convenience and customization, it is possible make unavailable any of the three sourcing methods,
#' e.g. hide the GEO method by not displaying the UI.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param menu Logical flag, should there be a menu for sourcing stored datasets? See details.
#' @param upload Logical flag, should users be able to upload data? See details.
#' @param GEO Logical flag, should users be able to pull data from GEO? (beta) See details.
#' @export
multiVCtrlUI <- function(id, menu = T, upload = T, GEO = T) {
  ns <- NS(id)
  tags$div(id = "multiVCtrlUI", style="margin-top:30px; margin-bottom:20px; margin-right:100px",
           if(menu) div(class = "forceInline", style = "margin-right: 40px;",
               selectizeInput(ns("dataset"), HTML("<strong>Available high-throughput datasets</strong>"),
                              choices = NULL, selected = NULL, multiple = T,
                              options = list(placeholder = "select one or more datasets to view"))),
           if(upload) div(class = "forceInline", style = "margin-right: 40px;", br(),
                          actionButton(ns("upload"), "Upload my data")),
           if(GEO) div(class = "forceInline", style = "margin-right: 40px;", br(),
                       actionButton(ns("getGEO"), "Source from GEO (beta)"))
          )
}

#' Shiny module server for controlling multi-column view
#'
#' \code{multiVCtrl} is the control hub that provides the data and parameters for \code{\link{multiVUI}},
#' \code{\link{geneV}} and \code{\link{selectV}}.
#'
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param cdata Data.table of low-throughput data.
#' @param hdlist A list containing high dimensional datasets; must have names to be used as choices in the selection menu.
#' @param choices Names referencing datasets in hdlist, to be used in selection menu.
#' When the datasets should be displayed as grouped, the choices can be passed in accordingly for \code{\link[shiny]{selectizeInput}}.
#' @param key Name of column that contains IDs in \preformatted{cdata} that link to samples in \preformatted{hdlist} datasets. Defaults to "ID".
#' @param checkFun A check function used for checking data uploads.
#' @param infoRmd Optional link to an Rmarkdown document containing details for the data upload module.
#' @return A list containing the data matrix for the parameter \preformatted{hdata} in the \code{\link{multiV}} module,
#' as well as parameters for \code{\link{geneV}} and \code{\link{selectV}}.
#' @export
multiVCtrl <- function(input, output, session,
                      cdata, hdlist, choices = names(hdlist), key = "ID", checkFun = NULL, infoRmd = NULL) {

  inview <- c()
  view <- reactiveValues(cdata = cdata, hddata = NULL)

  updateSelectizeInput(session, "dataset", choices = choices, selected = NULL)

  observe({
    if(!length(input$dataset)) {
      dataset <- list(NULL) # set return to NULL
      names(dataset) <-  paste0("i", which(names(hdlist) %in% inview))
      inview <<- c()
    } else {
      hdname <- setdiff(input$dataset, inview)
      if(length(hdname)) {
        dataset <- hdlist[hdname]
      } else {  # remove from view
        hdname <- setdiff(inview, input$dataset)
        dataset <- list(NULL)
      }
      inview <<- isolate(input$dataset)
      names(dataset) <- paste0("i", which(names(hdlist) %in% hdname)) # replace name with index # in hdlist
    }
    view$hddata <- dataset
  })

  observeEvent(input$upload, {
    showModal(
      modalDialog(dataUploadUI(session$ns("upload"), label = "<strong>Upload my data</strong>"),
                  includeMarkdown(infoRmd),
                  footer = modalButton("Cancel"))
    )
  })

  udata <- callModule(dataUpload, "upload")

  observeEvent(udata(), {
    # check which type of data
    data <- udata()
    if(key %in% names(data)) {
      # low-throughput processing: check and modify column names if necessary
      data <- merge(cdata, data, by = "ID", all = T)
      view$cdata <- data
    } else {
      # high-throughput processing
      filename <- attr(data, "filename")
      hdata <- as.matrix(data, rownames = 1)
      hdata <- t(hdata)

      hdata <- setNames(list(hdata), filename)
      hdlist <<- c(hdlist, hdata)
      choices$Uploaded <- c(choices$Uploaded, list(filename))
      updateSelectizeInput(session, "dataset", choices = choices, selected = filename)
    }
    removeModal()
  })

  observeEvent(input$getGEO, {
    showModal(
      modalDialog(getGEOInput(session$ns("GEO")),
                  footer = modalButton("Cancel"))
    )
  })

  GEOdata <- callModule(getGEOMod, "GEO")

  observeEvent(GEOdata, {
    removeModal()
  })


  return(view)
}

# Check fun, returns notification message
xpMatrixCheck <- function() {
  # check that IDs are nPOD IDs
  "Detected that at least some are not nPOD samples."
  "Detected that expression values are not annotated to gene Entrez IDs.
  Please use the Custom Selection when filtering with your data.
  Refer to upload guide for more details"
}
