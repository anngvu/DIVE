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
      list(type = "selectizeInput", id = "DataItem", args = list("DataItem", "DataItem", choices = O$OBI$Term, multiple = F)),
      list(type = "selectizeInput", id = "Level", args = list("Level", "Level", choices = CV$Level, multiple = F)),
      list(type = "selectizeInput", id = "CellTissue", args = list("CellTissue", "CellTissue", choices = O$CL$Term, multiple = T, options = list(maxOptions = 5))),
      list(type = "selectizeInput", id = "CellTissueContext", args = list("CellTissueContext", "CellTissueContext", choices = O$CL$Term, multiple = F, options = list(maxOptions = 5))),
      list(type = "selectizeInput", id = "EFOTerm", args = list("EFOTerm", "EFOTerm", choices = O$EFO$Term, multiple = T, options = list(maxOptions = 5))),
      list(type = "selectizeInput", id = "GOTerm", args = list("GOTerm", "GOTerm", choices = O$GO$Term, multiple = T)),
      list(type = "selectizeInput", id = "MPTerm", args = list("MPTerm", "MPTerm", choices = O$MP$Term, multiple = T))
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
collectDataInput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("inputs"))
}

#' Shiny module server for data collection
#'
#' Generates UI according to a template and returns input data. Fields can be pre-filled with existing data.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @param template UI template.
#' @param filldata A list of existing data values that can be passed in. If NULL, works as if in "add" instead of "edit" mode.
#' @param class Optional, class for ui CSS.
#' @export
collectData <- function(input, output, session,
                    template = reactive({ }), filldata = reactive({ }), class = "") { # class = "forceInline",

  edits <- reactiveVal(NULL)

  output$inputs <- renderUI({
    if(length(template())) {
      temp <- template()
      filldata <- filldata()
      for(i in seq_along(temp)) temp[[i]]$args[[1]] <- session$ns(temp[[i]]$args[[1]]) # ns(inputId)
      temp <- initInput(temp, filldata)
      ui <- lapply(temp, function(i) div(class = class, do.call(i$type, args = i$args)))
      tags$div(class = "forceInline card",
               ui,
               actionLink(session$ns("done"), label = "OK", icon = icon("check")))
    }
  })

  # Checking input$done as a roundabout way to check that inputs have been rendered;
  # selectize inputs have to be updated with server because client-side is extremely laggy when choices have thousannds of terms for ontologies
  observe({
    if(!is.null(input$done) && input$done == 0) {
      temp <- template() # whenever template changes
      filldata <- filldata() # whenever filldata changes
      for(i in seq_along(temp)) {
        if(temp[[i]]$type == "selectizeInput") {
          selected <- readCollapsed(filldata[[temp[[i]]$id]])
          updateSelectizeInput(session, inputId = temp[[i]]$args[[1]], choices = temp[[i]]$args$choices, selected = selected, server = T)
        }
      }
    }
  })

  observeEvent(input$done, {
    entries <- lapply(template(), function(x) if(x$type == "selectizeInput") writeCollapsed(input[[x$args[[1]]]]) else input[[x$args[[1]]]])
    names(entries) <- sapply(template(), function(x) x$id)
    edits(entries)
  })

  return(edits)

}

# Note: whenever data.table::fread actually implements sep2, this will change to take advantage of that
readCollapsed <- function(value, sep = "|") {
  ifelse(is.na(value), "", unlist(strsplit(value, split = sep, fixed = T)))
}

writeCollapsed <- function(value, sep = "|") {
  paste0(value, collapse = sep)
}

templateInput <- function(x, value) {
  if(x$type == "selectizeInput") {
    x$args$choices <- x$args$selected <- value
  } else {
    x$args$value <- value
  }
  x
}

# Handles populating fields with current data. If no current fill data, sets selectizeInput to be updated later.
initInput <- function(template, filldata = NULL) {
  if(length(filldata)) {
    # Note: If table is out of sync with template, fill data won't exist for template i$id
    values <- lapply(template, function(i) if(i$type == "selectizeInput") "" else filldata[[i$id]])
    Map(templateInput, template, values)
  } else {
    lapply(template, function(i) { if(i$type == "selectizeInput") i$args$choices <- ""; i })
  }
}

# fills in given data as value or choices depending on input type in template
# initInput1 <- function(template, filldata) {
#   values <- lapply(template, function(i) if(i$type == "selectizeInput") readCollapsed(filldata[[i$id]]) else filldata[[i$id]])
#   template <- Map(templateInput, template, values)
#   template
# }

# --------------------------------------------------------------------------------------------------------------------------#

#' Shiny module UI for reviewing and editing table data
#'
#' The UI features a basic table representation of the data and an interface for collecting data.
#'
#' @param id Character ID for specifying namespace, see \code{shiny::\link[shiny]{NS}}.
#' @export
reviewUI <- function(id, CSS = system.file("www/", "app.css", package = "DIVE")) {

  ns <- NS(id)
  fillPage(theme = shinythemes::shinytheme("paper"), if(!is.null(CSS)) includeCSS(CSS), padding = 20,
      fillRow(flex = c(0.25, 0.75),
      # column(3,
             # actionButton(ns("new"), "", icon = icon("plus")), br(),
             div(div(class = "forceInline", uiOutput(ns("nav"))), br(),
                 actionLink(ns("new"), "Entry", icon = icon("plus")), br(),
                 collectDataInput(ns("edit"))
                 ),
             #),
             div(DT::dataTableOutput(ns("table")))
            )
  )
}

#' Shiny module server for reviewing and editing table data
#'
#' The module composes the table data with the ability to edit current or new rows.
#' The table is connected to the collectData module,
#' which provides a data collection interface using a data template, the overall UI allows
#' editing a current or new row of data.
#'
#' @param input,output,session Standard \code{shiny} boilerplate.
#' @inheritParams collectData
#' @param nav For templates containing multiple parts (sections),
#' this determines whether a selectInput or radioButtons type navigation menu
#' is generated for selecting which part of the template to be reviewed.
#' It is not useful for templates of length 1.
#' @export
review <- function(input, output, session, template, dt, nav = c("radioButtons", "selectInput")) {

  dt <- reactiveVal(dt)
  filldata <- reactiveVal(NULL)
  active <- reactiveVal(1)

  output$nav <- renderUI({
    if(length(template) > 1) {
      if(nav == "radioButtons") {
        radioButtons(session$ns("section"), label = "Section", choices = names(template), inline = T)
      } else {
        selectInput(session$ns("section"), label = "Section", choices = names(template))
      }
    }
  })

  observeEvent(input$section, {
    active(input$section)
  })

  temp <- reactive({
    template[[active()]]
  })

  edits <- callModule(collectData, "edit", template = temp, filldata = filldata)

  observeEvent(edits(), {
    edits <- edits()
    dt_ <- copy(dt())
    for(col in names(edits)) dt_[input$table_rows_selected, col] <- edits[[col]]
    dt(dt_)
  })

  observe({
    selected <- input$table_rows_selected
    if(length(selected)) {
      values <- as.list(dt()[selected, ])
      filldata(values)
    } else {
      filldata(NULL)
    }
  })

  observeEvent(input$new, {
    filldata(NULL)
  })


  output$table = DT::renderDataTable({
    showcols <- which(!(names(dt()) %in% sapply(temp(), function(x) x$id)))
    DT::datatable(dt(), options = list(pageLength = 15, columnDefs = list(list(visible = F, targets = showcols))),
                  style = "bootstrap", selection = "single")
  })
}

#' Launch Shiny app for data curation
#'
#' Launch an interface to view and modify curated data
#'
#' @export
curateR <- function() {
  ui <- reviewUI("method")
  server <- function(input, output, session) { callModule(review, id = "method", template = templateMetadata(), dt = metadata) }
  shinyApp(ui, server)
}


