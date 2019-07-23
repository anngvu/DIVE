#-- Sections ----------------------------------------------------------------------------------------------------------------------------#

# C - Core annotations
# O - Ontology-focused annotations
# M - Methods
# E - Extra notes
# T - Tracking: availability, in-app sharing, last reviewed, source

# To do: schema to template function?
templateMetadata <- function() {
  list(
    Core = list(
      list(type = "selectizeInput", id = "Source", args = list("Source", "Source", choices = metadata$Source, multiple = F, options = list(create = T))),
      # list(type = "textInput", args = list("Contributor")),
      # list(type = "textInput", args = list("VarID", "VarID")),
      list(type = "textInput", id = "Variable", args = list("Variable", "Variable")),
      list(type = "textInput", id = "Value", args = list("Value", "Value")),
      list(type = "textInput", id = "Levels", args = list("Levels", "Levels")),
      list(type = "textInput", id = "Description", args = list("Description", "Description")),
      list(type = "numericInput", id = "Dimensions", args = list("Dimensions", "Dimensions", value = 1))
      #list(type = "checkboxInput", id = "LinkedID", args = list("LinkedID", "LinkedID")),
      #list(type = "checkboxInput", id  = "IndividualLevelData", args = list("IndividualLevelData", "IndividualLevelData")),
      #list(type = "checkboxInput", id = "Complex", args = list("Complex", "Complex"))
    ),
    # Technical curator annotations -- controlled vocabulary from ontologies and coded values
    Ontology = list(
      list(type = "selectizeInput", id = "Level", args = list("Level", "Level", choices = CV$Level, multiple = F)),
      list(type = "selectizeInput", id = "CellTissue", args = list("CellTissue", "CellTissue", choices = O$CL$Term, multiple = T, options = list(maxOptions = 5))),
      list(type = "selectizeInput", id = "CellTissueContext", args = list("CellTissueContext", "CellTissueContext", choices = O$CL$Term, multiple = T, options = list(maxOptions = 5))),
      list(type = "selectizeInput", id = "EFOTerm", args = list("EFOTerm", "EFOTerm", choices = O$CL$Term, multiple = T, options = list(maxOptions = 5)))
      # list(type = "selectizeInput", args = list("GOTerm", "GOTerm", choices = O$GO$Term, multiple = T))
      # list(type = "selectizeInput", args = list("HPOTerm", "HPOTerm", choices = O$HPO$Term, multiple = T))
    ),
    Method = list(
      list(type = "selectizeInput", id = "MethodID", args = list("MethodID", "MethodID", choices = methods$MethodID))
    ),
    # extra thematic curator+researcher annotation
    Extra = list(
      list(type = "selectizeInput", id = "ThemeTag", args = list("ThemeTag", "ThemeTag", choices = unique(metadata$ThemeTag), options = list(create = T))),
      list(type = "textInput", id = "VarWithinD", args = list("VarWithinD", "VarWithinD")),
      list(type = "textInput", id = "VarDvsCtrl", args = list("VarDvsCtrl", "VarDvsCtrl")),
      list(type = "textInput", id = "Relevance", args = list("Relevance", "Relevance")),
      list(type = "textInput", id = "PrivateNote", args = list("PrivateNote", "PrivateNote")),
      list(type = "textInput", id = "PublicNote", args = list("PublicNote", "PublicNote"))
    ),
    Tracking = list(
      list(type = "textInput", id = "DataSource", args = list("DataSource", "DataSource")),
      list(type = "textInput", id = "DataSourceLink", args = list("DataSourceLink", "DataSourceLink"))
      # list(type = "textOutput", id = "InApp", args = list("InApp")),
      # list(type = "textOutput", id = "LastModified", args = list("LastModified"))
    )
  )
}

templateMethod <- function() {
  list(
    Method = list(
      # list(type = "textOutput", args = list("MethodID")),
      list(type = "textInput", args = list("Method", "Method")),
      list(type = "selectizeInput", args = list("OBITerm", "OBITerm", choices = O$OBI$Name, multiple = T)),
      list(type = "textInput", args = list("MethodCitation", "MethodCitation")),
      list(type = "textInput", args = list("MethodResources", "MethodResources")),
      list(type = "textAreaInput", args = list("MethodDetails", "MethodDetails"))
    )
  )
}


#' Shiny module UI for data collection
#'
#' UI contains input fields to collect data.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
collectInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("inputs"))
}

#' Shiny module server for data collection
#'
#' Generates UI according to a template and returns input data. Fields can be pre-filled with existing data.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param template UI template.
#' @param filldata A list of existing data values.
#' @param nav Type of top-level input to navigate sections of the template, or NULL if everything in template should be displayed all in one page.
#' @param class Optional, class for ui CSS.
#' @export
collect <- function(input, output, session,
                    template = reactive({ }), filldata = reactive({ }), nav = "radioButtons", class = "") { # class = "forceInline",

  output$inputs <- renderUI({
    if(length(template())) {
      temp <- template()
      for(i in seq_along(temp)) temp[[i]]$args[[1]] <- session$ns(temp[[i]]$args[[1]])
      if(length(filldata())) temp <- initializeInput1(temp, filldata()) else temp <- initializeInput0(temp)
      ui <- lapply(temp, function(i) div(class = class, do.call(i$type, args = i$args)))
      tags$div(class = "forceInline card", ui, actionLink(session$ns("done"), label = "OK", icon = icon("check")))
    }
  })

  # Checking input$done as a roundabout way to check that inputs have been rendered;
  # selectize inputs have to be updated with server because client-side is extremely laggy when choices are ontologies
  observe({
    if(!is.null(input$done) && input$done == 0) {
      temp <- template()
      for(i in seq_along(temp)) {
        if(temp[[i]]$type == "selectizeInput") updateSelectizeInput(session, inputId = temp[[i]]$args[[1]], choices = temp[[i]]$args$choices, server = T)
      }
    }
  })

  edits <- reactive({
    reactiveValuesToList(input)
  })

  observeEvent(input$done, {
    return(edits)
  })

}

# Note: whenever data.table::fread actually implements sep2, this will change to take advantage of that
readCollapsed <- function(value, sep = "|") {
  unlist(strsplit(value, split = sep, fixed = T))
}

templateInput <- function(x, value) {
  if(x$type == "selectizeInput") {
    x$args$choices <- x$args$selected <- value
  } else {
    x$args$value <- value
  }
  x
}

# Set selectizeInput to be updated later
initializeInput0 <- function(template) {
  lapply(template, function(i) { if(i$type == "selectizeInput") i$args$choices <- ""; i })
}

# fills in given data as value or choices depending on input type in template
initializeInput1 <- function(template, filldata) {
  values <- lapply(template, function(i) if(i$type == "selectizeInput") readCollapsed(filldata[[i$id]]) else filldata[[i$id]])
  template <- Map(templateInput, template, values)
  template
}

# --------------------------------------------------------------------------------------------------------------------------#

reviewUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"), if(!is.null(CSS)) includeCSS(CSS),
    uiOutput(ns("nav")),
    actionButton(ns("new"), "", icon = icon("plus")),
    collectInput(ns("edit")),
    div(style = "margin-top: 30px;", DT::dataTableOutput(ns("table")))
  )
}

review <- function(input, output, session, template, mydata) {

  filldata <- reactiveVal(NULL)

  output$nav <- renderUI({
    radioButtons(session$ns("section"), label = "Section", choices = names(template), inline = T)
  })

  temp <- reactive({
    req(input$section)
    template[[input$section]]
  })

  edits <- callModule(collect, "edit", template = temp, filldata = filldata)


  observe({
    selected <- input$table_rows_selected
    if(length(selected)) {
      values <- as.list(mydata[selected, ])
      filldata(values)
    } else {
      filldata(NULL)
    }
  })

  observeEvent(input$new, {
    filldata(NULL)
  })

  output$table = DT::renderDataTable({
    DT::datatable(mydata, options = list(pageLength = 15), rownames = F, style = "bootstrap", selection = "single")
  })
}

curateUI <- function(id) {
  ns <- NS(id)
  reviewUI(ns("method"))
}

curate <- function(input, output, session) {

  callModule(review, id = "method", template = templateMetadata(), mydata = metadata)
}

#' Launch Shiny app for metadata curation
#'
#' Launch an interface to view and modify metadata of curated data
#'
#' @export
curateR <- function() {
  ui <- curateUI("main")
  server <- function(input, output, session) { callModule(curate, "main") }
  shinyApp(ui, server)
}


