#' Shiny module UI for controlling multi-column views
#'
#' Shiny module UI for controlling multi-column views
#'
#' Multiple high dimensional datasets can be displayed by calling separate instances of
#' \code{\link{xVUI}} modules, with the contents typically laid out in row containers.
#' This controller UI has three different ways to source the datasets:
#' \enumerate{
#'   \item Selecting from available pre-processed datasets.
#'   \item User-uploaded data.
#'   \item A beta (least-supported) method of retrieving datasets from GEO.
#' }
#' For convenience and customization, it is possible make unavailable any of the three sourcing methods,
#' e.g. hide the GEO sourcing option by not displaying the UI.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param menu Logical flag, whether to allow a menu for loading stored datasets.
#' @param upload Logical flag, whether to allow data upload.
#' @param GEO Logical flag, whether to allow pulling data from GEO (beta).
#' @export
multiVCtrlUI <- function(id, menu = TRUE, upload = TRUE, GEO = TRUE) {
  ns <- NS(id)
  tags$div(class = "multiVCtrlUI-panel", id = ns("multiVCtrlUI"),
           if(menu) div(class = "ui-inline",
                        selectizeInput(ns("dataset"), HTML("<strong>Available datasets</strong>"),
                                       choices = NULL, selected = NULL, multiple = T,
                                       options = list(placeholder = "select to view"))),
           if(upload) div(class = "ui-inline", br(), actionButton(ns("upload"), "Upload my data")),
           if(GEO) div(class = "ui-inline", br(), actionButton(ns("getGEO"), HTML("Source from GEO <sup>beta</sup>")))
  )
}

#' Shiny module server for controlling multi-column views
#'
#' Control hub that provides data and parameters for \code{\link{xVServer}},
#' \code{\link{geneVServer}} and \code{\link{selectVServer}}
#'
#' The server logic handles sourcing of large expression datasets with three different methods:
#' \enumerate{
#'   \item Selecting from available pre-processed datasets.
#'   \item User-uploaded data.
#'   \item A beta (least-supported) method of retrieving datasets from GEO.
#' }
#' The data in \code{cdata} is supposed to be a phenotype or clinical
#' feature that one usually tries to correlate with expression data and can be numeric or categorical.
#' The module handles upload of phenotype/clinical data,
#' using a mutable version of \code{cdata} that appends user uploaded data.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param hdlist A list of matrices representing high dimensional datasets; the names are used for \code{choices}.
#' @param choices A list of choices to populate selection menu, defaults to names of datasets in \code{hdlist}.
#' When dataset selection should be displayed under different groups,
#' one can pass in a list formatted accordingly for \code{shiny::\link[shiny]{selectizeInput}}.
#' @param cdata A \code{data.table} of characteristics data, commonly phenotype or clinical data.
#' @param key Name of column that contains IDs in \code{cdata} matching sample IDs in \code{hdlist} datasets. Defaults to "ID".
#' @param preselect Optional, pre-selected phenotype or clinical variables from \code{cdata}.
#' @inheritParams dataUploadServer
#' @return A reactive values list containing the data matrix
#' for the parameter \preformatted{hdata} of the \code{\link{multiVServer}} module,
#' as well as parameters for \code{\link{geneVServer}} and \code{\link{selectVServer}}.
#' @import shiny
#' @export
multiVCtrlServer <- function(id,
                             hdlist, choices = names(hdlist),
                             cdata,
                             key = "ID",
                             checkFun = NULL,
                             preselect = NULL,
                             informd = system.file("info/ht_upload.Rmd", package = "DIVE")) {

  moduleServer(id, function(input, output, session) {

    # cdata key should be character for later merging with hdata
    cdata[[key]] <- as.character(cdata[[key]])
    view <- reactiveValues(cdata = cdata, hdlist = hdlist, hdata = NULL, vselect = preselect)
    inview <- c()

    updateSelectizeInput(session, "dataset", choices = choices, selected = NULL)

    # Parse a URL request for a specific dataset
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if(!is.null(query[["dataset"]])) updateSelectInput(session, "dataset", selected = query[["dataset"]])
    })

    # Handle dataset selection or de-selection ------------------------------------------------------------------#
    observe({
      if(!length(input$dataset)) { # everything has been cleared from the global dataset selection
        dataset <- stats::setNames(object = list(NULL), # set return to NULL
                            nm = paste0("i", which(names(view$hdlist) %in% inview)))
        inview <<- c()
      } else {
        dsname <- setdiff(input$dataset, inview)
        if(length(dsname)) { # if more in selection than in view, view needs to add new dataset
          dataset <- stats::setNames(object = view$hdlist[dsname],
                              paste0("i", which(names(view$hdlist) %in% dsname)))
        } else {  # a dataset needs to be removed from view
          dsname <- setdiff(inview, input$dataset)
          dataset <- stats::setNames(object = list(NULL),
                              paste0("i", which(names(view$hdlist) %in% dsname)))
        }
        inview <<- isolate(input$dataset)
      }
      view$hdata <- dataset
    })

    # -- packaging dataset/adding to selection -----------------------------------------------------------------#

    addDataToSelection <- function(dataset, label, selectgroup) {
      dataset <- t(dataset)
      dataset <- stats::setNames(list(dataset), label)
      view$hdlist <- c(view$hdlist, dataset)
      choices[[selectgroup]] <<- c(choices[[selectgroup]], list(label))
      updateSelectizeInput(session, "dataset", choices = choices, selected = c(input$dataset, label))
    }

    # -- handling user-uploaded data --------------------------------------------------------------------------#

    udata <- dataUploadServer("upload")

    observeEvent(input$upload, {
      showModal(modalDialog(title = "Upload my data",
          dataUploadUI(session$ns("upload"), label = NULLL),
          includeMarkdown(informd),
          footer = modalButton("Cancel")
      ))
    })

    observeEvent(udata(), {
      dataset <- udata() # check whether uploaded expression data or phenodata
       if(key %in% names(dataset)) {
        # phenodata -> check and modify column names if necessary
        dataset <- merge(cdata, dataset, by = key, all = T)
        view$cdata <- dataset
        removeModal()
      } else {
        # high-throughput processing
        filename <- attr(dataset, "filename")
        # If filename is same as something in the selection, upload will replace that object (!)
        filename <- paste0("userdata_", filename)
        dataset <- as.matrix(dataset, rownames = 1)
        addDataToSelection(dataset, label = filename, selectgroup = "Uploaded")
        removeModal()
      }
    })


    # -- handling GEO data -----------------------------------------------------------------------------------#

    GEOdata <- getGEOServer("GEO")

    observeEvent(input$getGEO, {
      showModal(modalDialog(title = "Get data from GEO",
                            getGEOInput(session$ns("GEO")),
                            footer = modalButton("Cancel")
               ))
    })

    # When GEO data is pulled successfully, GEOdata$return changes from NULL to TRUE
    observeEvent(GEOdata$return, {
      addDataToSelection(GEOdata$eset, label = GEOdata$accession, selectgroup = "GEO")
      if(!is.null(GEOdata$pData)) {
        pData <- GEOdata$pData
        # add key column to pData for merge even though the samples can be
        # unrelated and there might not be anything to merge upon
        for(col in names(pData)) pData[[col]] <- factor(pData[[col]])
        pData[[key]] <- rownames(pData)
        data <- merge(cdata, pData, by = key, all = T)
        view$cdata <- data
        view$vselect <- names(pData)[1]
      } else {
        view$vselect <- NULL
      }
      updateSelectizeInput(session, "dataset", choices = choices, selected = GEOdata$accession)
    }, ignoreInit = TRUE)

    return(view)
  })

}

# TO-DO
# Check fun, returns notification message
xpMatrixCheck <- function() {
  # check that IDs are same as main IDs
  "Detected that at least some are not nPOD samples."
  "Detected that expression values are not annotated to gene Entrez IDs.
  Please use the Custom Selection when filtering with your data.
  Refer to upload guide for more details"
}
