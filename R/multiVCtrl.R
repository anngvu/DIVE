#' Shiny module UI for controlling multi-column view
#'
#' Shiny module UI for controlling multi-column view
#'
#' Multiple high dimensional datasets can be displayed by calling separate instances of \code{\link{multiVUI}} modules,
#' with the contents typically laid out in separate a row containers. This "controller" UI establishes three different ways to
#' select and source the datasets:
#' \enumerate{
#'   \item Selecting from available pre-processed datasets.
#'   \item User-uploaded data.
#'   \item A beta (least-supported) method of retrieving datasets from GEO.
#' }
#' For convenience and customization, it is possible make unavailable any of the three sourcing methods,
#' e.g. hide the GEO method by not displaying the UI.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @param menu Logical flag, should there be a menu for sourcing stored datasets? See details.
#' @param upload Logical flag, should users be able to upload data? See details.
#' @param GEO Logical flag, should users be able to pull data from GEO? (beta) See details.
#' @export
multiVCtrlUI <- function(id, menu = TRUE, upload = TRUE, GEO = TRUE) {
  ns <- NS(id)
  tags$div(class = "multiVCtrlUI", id = ns("multiVCtrlUI"),
           if(menu) div(class = "forceInline", style = "margin-right: 40px;",
                        selectizeInput(ns("dataset"), HTML("<strong>Available datasets</strong>"),
                                       choices = NULL, selected = NULL, multiple = T,
                                       options = list(placeholder = "select one or more to view"))),
           if(upload) div(class = "forceInline", style = "margin-right: 40px;", br(),
                          actionButton(ns("upload"), "Upload my data")),
           if(GEO) div(class = "forceInline", style = "margin-right: 40px;", br(),
                       actionButton(ns("getGEO"), HTML("Source from GEO <sup>beta</sup>")))
  )
}

#' Shiny module server for controlling multi-column view
#'
#' \code{multiVCtrl} is the control hub that provides data and parameters for \code{\link{multiVUI}},
#' \code{\link{geneV}} and \code{\link{selectV}}.
#'
#' The server logic handles sourcing of large expression datasets with three different methods:
#' \enumerate{
#'   \item Selecting from available pre-processed datasets.
#'   \item User-uploaded data.
#'   \item A beta (least-supported) method of retrieving datasets from GEO.
#' }
#' The module also handles upload of phenotype/clinical data. The module is instantiated with whatever is
#' passed into the parameter \code{cdata} and uses a mutable version of \code{cdata} that
#' appends user uploads. The data in \code{cdata} is supposed to be a phenotype or clinical
#' feature that one usually tries to correlate with expression data and can be numeric or categorical.
#' By default, character columns as well as ones matching \code{factorx} in \code{cdata}
#' will be converted to factors.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param cdata Data.table of phenotype or clinical data.
#' @param hdlist A list of matrices representing high dimensional datasets; the names are used for \code{choices}.
#' @param choices Names referencing datasets in \code{hdlist} to be used in selection menu,
#' taken from the names in \code{hdlist} by default. However, when dataset selection should be displayed under different groups,
#' one can pass in a list formatted accordingly for \code{\link[shiny]{selectizeInput}}.
#' @param key Name of column that contains IDs in \code{cdata} that link to samples in \code{hdlist} datasets. Defaults to "ID".
#' @param checkFun Optional, a check function used for checking data uploads.
#' @param factorx If not NULL, the given name pattern will be used for recognizing and converting columns in cdata to factor. See details.
#' @param preselect Optional, pre-selected phenotype or clinical variables from \code{cdata}.
#' @param infoRmd Optional link to an Rmarkdown document containing description and/or instructions for data upload option.
#' @return A reactive values list containing the data matrix for the parameter \preformatted{hdata} of the \code{\link{multiV}} module,
#' as well as parameters for \code{\link{geneV}} and \code{\link{selectV}}.
#' @export
multiVCtrlServer <- function(id,
                             cdata, hdlist, choices = names(hdlist),
                             key = "ID",
                             checkFun = NULL,
                             factorx = NULL,
                             preselect = NULL,
                             infoRmd = system.file("help/ht_upload.Rmd", package = "DIVE")) {

  moduleServer(id, function(input, output, session) {

    inview <- c()
    view <- reactiveValues(cdata = cdata, hdlist = hdlist, hdata = NULL, vselect = preselect)

    updateSelectizeInput(session, "dataset", choices = choices, selected = NULL)

    # Parse a URL request for a specific dataset
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if(!is.null(query[["dataset"]])) updateSelectInput(session, "dataset", selected = query[["dataset"]])
    })


    # Handle dataset selection or de-selection
    observe({
      if(!length(input$dataset)) { # everything has been cleared from the global dataset selection
        dataset <- setNames(object = list(NULL), # set return to NULL
                            nm = paste0("i", which(names(view$hdlist) %in% inview)))
        inview <<- c()
      } else {
        hdname <- setdiff(input$dataset, inview)
        if(length(hdname)) { # a new dataset needs to be added to view
          dataset <- setNames(object = view$hdlist[hdname],
                              paste0("i", which(names(view$hdlist) %in% hdname)))
        } else {  # a dataset needs to be removed from view
          hdname <- setdiff(inview, input$dataset)
          dataset <- setNames(object = list(NULL),
                              paste0("i", which(names(view$hdlist) %in% hdname)))
        }
        inview <<- isolate(input$dataset)
      }
      view$hdata <- dataset
    })

    # -- handling user-uploaded data --------------------------------------------------------------------------#
    observeEvent(input$upload, {
      showModal(
        modalDialog(dataUploadUI(session$ns("upload"), label = "<strong>Upload my data</strong>"),
                    includeMarkdown(infoRmd),
                    footer = modalButton("Cancel"))
      )
    })

    udata <- dataUploadServer("upload")

    observeEvent(udata(), {
      # check which type of data
      data <- udata()
      if(key %in% names(data)) {
        # low-throughput processing: check and modify column names if necessary
        data <- merge(cdata, data, by = key, all = T)
        if(!is.null(factorx)) for(i in grep(factorx, names(data))) data[[i]] <- factor(data[[i]])
        view$cdata <- data
        removeModal()
      } else {
        # high-throughput processing
        filename <- attr(data, "filename")
        # If the filename is the same as something currently in the selection,
        # the uploaded data will replace the current object (!), which we currently disallow
        if(filename %in% names(view$hdlist)) {
          showNotification("Dataset with same file name already exists (overwrites not allowed).
                           To upload a different version, change file name to reflect the version.",
                           type = "warning", duration = NULL)
        } else {
          hdata <- as.matrix(data, rownames = 1)
          hdata <- t(hdata)
          hdata <- setNames(list(hdata), filename)
          view$hdlist <- c(view$hdlist, hdata)
          choices$Uploaded <<- c(choices$Uploaded, list(filename))
          updateSelectizeInput(session, "dataset", choices = choices, selected = c(input$dataset, filename))
          removeModal()
        }
      }
    })

    # -- handling GEO data -----------------------------------------------------------------------------------#
    observeEvent(input$getGEO, {
      showModal(
        modalDialog(title = "Get data from GEO",
                    getGEOInput(session$ns("GEO")),
                    footer = modalButton("Cancel"))
      )
    })

    GEOdata <- getGEOServer("GEO")

    # When GEO data is pulled successfully, GEOdata$call changes from NULL to 1
    observeEvent(GEOdata$call, {
      hdata <- t(GEOdata$eset)
      # showModal(
      #   modalDialog(title = "Status",
      #               )
      # )
      hdata <- setNames(list(hdata), GEOdata$accession)
      view$hdlist <- c(view$hdlist, hdata)
      choices$GEO <- c(choices$GEO, list(GEOdata$accession))
      if(!is.null(GEOdata$pData)) {
        pData <- GEOdata$pData
        # add key column to pData so we can merge; the samples can be totally
        # unrelated and there might not be anything to merge upon;
        # tell user results of ID and annotation checks
        for(col in names(pData)) pData[[col]] <- factor(pData[[col]])
        pData[[key]] <- rownames(pData)
        data <- merge(cdata, pData, by = key, all = T)
        view$cdata <- data
        view$vselect <- names(pData)[1]
      } else {
        view$vselect <- NULL
      }
      updateSelectizeInput(session, "dataset", choices = choices, selected = GEOdata$accession)
      removeModal()
    })

    return(view)
  })

}

# TO-DO
# Check fun, returns notification message
xpMatrixCheck <- function() {
  # check that IDs are nPOD IDs
  "Detected that at least some are not nPOD samples."
  "Detected that expression values are not annotated to gene Entrez IDs.
  Please use the Custom Selection when filtering with your data.
  Refer to upload guide for more details"
}
