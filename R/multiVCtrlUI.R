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
  tags$div(class = "multiVCtrlUI", id = ns("multiVCtrlUI"),
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
#' The server logic handles sourcing of high-throughput datasets with three different methods:
#' \enumerate{
#'   \item Selecting datasets that are pre-processed and stored in memory.
#'   \item User-uploaded data.
#'   \item A beta (and least-supported) method of retrieving datasets from GEO.
#' }
#' The module also handles upload of low-throughput data. The module is instantiated with whatever is
#' passed into the parameter \code{cdata}. The module uses a mutable version of \code{cdata} that
#' can change based on user uploads. The data in \code{cdata} is supposed to be a phenotype or clinical
#' feature that one usually tries to correlate with expression data and can be numeric or categorical.
#' By default, any column that is character as well as what matches \code{factorx} in \code{cdata}
#' will be converted to factors.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param cdata Data.table of "low-throughput" phenotype or clinical data.
#' @param hdlist A list containing high dimensional datasets; must have names to be used as choices in the selection menu.
#' @param choices Names referencing datasets in hdlist, to be used in selection menu.
#' When the datasets should be displayed as grouped, the choices can be passed in accordingly for \code{\link[shiny]{selectizeInput}}.
#' @param key Name of column that contains IDs in \preformatted{cdata} that link to samples in \preformatted{hdlist} datasets. Defaults to "ID".
#' @param vselect A default selected column in \preformatted{cdata} to display.
#' @param checkFun Optional, a check function used for checking data uploads.
#' @param factorx If not NULL, the given name pattern will be used for recognizing and converting columns in cdata to factor. See details.
#' @param infoRmd Optional link to an Rmarkdown document containing details for the data upload module.
#' @return A \code{view} object which is a list containing the data matrix for the parameter \preformatted{hdata} of the \code{\link{multiV}} module,
#' as well as parameters for \code{\link{geneV}} and \code{\link{selectV}}.
#' @export
multiVCtrl <- function(input, output, session,
                      cdata, hdlist, choices = names(hdlist),
                      key = "ID", vselect = "donor.type",
                      checkFun = NULL,
                      factorx = NULL,
                      infoRmd = system.file("help/ht_upload.Rmd", package = "DIVE")) {

  inview <- c()
  view <- reactiveValues(cdata = cdata, hdlist = hdlist, hddata = NULL, vselect = vselect)

  updateSelectizeInput(session, "dataset", choices = choices, selected = NULL)

  # parse a URL request for a specific dataset
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[["dataset"]])) updateSelectInput(session, "dataset", selected = query[["dataset"]])
  })

  observe({
    if(!length(input$dataset)) { # when everything has been cleared from the global dataset selection
      dataset <- list(NULL) # set return to NULL
      names(dataset) <-  paste0("i", which(names(view$hdlist) %in% inview))
      inview <<- c()
    } else {
      hdname <- setdiff(input$dataset, inview)
      if(length(hdname)) { # a new dataset needs to be added to view
        dataset <- view$hdlist[hdname]
      } else {  # a dataset needs to be removed from view
        hdname <- setdiff(inview, input$dataset)
        dataset <- list(NULL)
      }
      inview <<- isolate(input$dataset)
      names(dataset) <- paste0("i", which(names(view$hdlist) %in% hdname)) # replace name with index # in hdlist
    }
    view$hddata <- dataset
  })

  # -- handling user-uploaded data --------------------------------------------------------------------------#
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
      data <- merge(cdata, data, by = key, all = T)
      if(!is.null(factorx)) for(i in grep(factorx, names(data))) data[[i]] <- factor(data[[i]])
      view$cdata <- data
      removeModal()
    } else {
      # high-throughput processing
      filename <- attr(data, "filename")
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
        # view$vselect <- NULL # to do: more sophisticated to infer most appropriate vselect as default
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

  GEOdata <- callModule(getGEOMod, "GEO")

  observeEvent(GEOdata$call, {
    hdata <- t(GEOdata$eset)
    hdata <- setNames(list(hdata), GEOdata$accession)
    view$hdlist <- c(view$hdlist, hdata)
    choices$GEO <- c(choices$GEO, list(GEOdata$accession))
    updateSelectizeInput(session, "dataset", choices = choices, selected = GEOdata$accession)
    if(!is.null(GEOdata$pData)) {
      pData <- GEOdata$pData
      pData[[key]] <- rownames(pData)
      data <- merge(cdata, pData, by = "ID", all = T)
      view$cdata <- data
      view$vselect <- names(pData)[1]
    } else {
      view$vselect <- NULL
    }
    removeModal()
  })


  return(view)
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
