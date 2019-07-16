# Gives metadata fields in metadata
# template <- list(
#   Availability = list(
#     list(id = "SourceType", input = "text", label = "Source Type"),
#     list(id = "Publication", input = "text", label = "Source Type"),
#     list(id = "Reference", input = "text", label = "Reference")
#   )
# )

CurateBasicInput <- function(id) {
  ns <- NS(id)
  tags$div(class = "forceInline",
    # Basic data fields
    div(class = "forceInline", textInput(ns("SourceType"), "Source Type")), # nPOD Core vs. Independent Experiment?
    div(class = "forceInline", textInput(ns("Publication"), "Source Publication")),
    div(class = "forceInline", textInput(ns("Reference"), "Reference")), # as in Li et al. 2009; calculate in the backend?
    # div(class = "forceInline", textInput(ns("People"), "People")), # this is calculated in the backend and allows searching by person, not just publication
    # div(class = "forceInline", textInput(ns("VarID"), "Variable ID")), # this is assigned in the backend
    div(class = "forceInline", textInput(ns("Variable"), "Variable Name")),
    div(class = "forceInline", selectizeInput(ns("Value"), "Variable Value Type", choices = "", options = list(create = T))), # Should be associated with Method
    div(class = "forceInline", textInput(ns("Description"), "Variable Description")),
    div(class = "forceInline", numericInput(ns("Dimensions"), "Dimensions", value = 1, min = 1))
  )
}

CurateBasic <- function(input, output, session,
                   metadata = metadata) {

}



curateUI_2 <- function(id) {
  ns <- NS(id)
  # Data availability and lineage; re-derived/transformed/summarized/etc. data has a parent
  tags$div(class = "forceInline",
           div(class = "forceInline", selectInput(ns("IndividualLevelData"), "Individual-level Data", choices = c("Yes", "No", "Unknown"))),
           div(class = "forceInline", selectInput(ns("Shared"), "Shared", c("Yes", "No/Contact author"))),
           div(class = "forceInline", selectInput(ns("Derived"), "Derived From", choices = ""))
           # Data dividends -- How data is derived should be in methods -- data parent goes here
           # Derivation method should be in methods
  )
}

curateUI_3 <- function(id) {
  ns <- NS(id)
  # Technical curator annotations -- ontology-based, coded values
  tags$div(class = "forceInline",
           div(class = "forceInline", selectInput(ns("Level"), "Level", choices = "")),
           div(class = "forceInline", selectInput(ns("CellTissue"), "Cell or Tissue", choices = O$CL$Name)),
           div(class = "forceInline", selectInput(ns("TissueContext"), "Tissue Context", choices = O$CL$Name)),
           div(class = "forceInline", selectizeInput(ns("EFO"), "EFO Annotation", choices = "")),
           div(class = "forceInline", selectizeInput(ns("GO"), "GO Process Annotation", choices = O$GO$Name)),
           div(class = "forceInline", selectizeInput(ns("Entrez"), "Entrez Gene IDs", choices = "")),
           div(class = "forceInline", fileInput(ns("Entrez_file"), "Entrez Gene IDs")),
           # Methods
           div(class = "forceInline", selectInput(ns("MethodRef"), "Method Reference", choices = "")),
           div(class = "forceInline", actionButton(ns("addMethod"), "Method")), # shows input fields for adding new method
           div(class = "forceInline", uiOutput(ns("inputMethod"), inline = T))
  )
}

curateUI_4 <- function(id) {
  ns <- NS(id)
  # Thematic curator+researcher annotation -- coded values, free text
  tags$div(class = "forceInline",
           div(class = "forceInline", textInput(ns("Tags"), "Tags")),
           div(class = "forceInline", textInput(ns("VarWithinD"), "Variance within Disease")),
           div(class = "forceInline", textInput(ns("VarDvsCtrl"), "Variance Disease vs Control")),
           div(class = "forceInline", textInput(ns("Relevance"), "Relevance Summary")),
           div(class = "forceInline", textInput(ns("PrivateNote"), "PrivateNote")),
           div(class = "forceInline", textInput(ns("PublicNote"), "Note"))
  )
}

curateUI_5 <- function(id) {
  ns <- NS(id)
  # Additional tracking and metrics
  tags$div(class = "forceInline",
           div(class = "forceInline", textInput(ns("LastReviewed"), "Last Reviewed")),
           div(class = "forceInline", textInput(ns("DataSource"), "Source Figure or Table")),
           div(class = "forceInline", textInput(ns("DataSourceLink"), "Source Link"))
  )
}


curateUI <- function(id) {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
    fluidRow(radioButtons(ns("section"), "Section", choices = c("Availability", "Coded", "Other annotation", "Metrics"), inline = T)),
    fluidRow(DT::dataTableOutput(ns("table"))),
    fluidRow(actionButton(ns("submit"), "Submit"))
  )
}

curate <- function(input, output, session
                   ) {

  output$table = DT::renderDataTable({
    DT::datatable(metadata[, 1:10], options = list(pageLength = 15), rownames = F, style = "bootstrap")
  })
  # output$inputMethod <- renderUI({
  #   input$addMethod
  #   tagList(
  #     div(class = "forceInline", selectInput(session$ns("Method"), "Method", choices = O[["OBI"]][["Name"]])),
  #     div(class = "forceInline", textInput(session$ns("MethodDetails"), "Method Details")),
  #     div(class = "forceInline", actionButton(session$ns("saveMethod"), "Save"))
  #   )
  # })
  #
  # observeEvent(input$saveMethod, {
  #   # create MethodRef and save into Methods file
  #   MethodRef <- paste0(input$Reference, "::", input$Method)
  #   new <- list(MethodRef = MethodRef, Method = input$Method, MethodDetails = input$MethodDetails)
  #   Methods <<- rbind(Methods, new)
  #   updateSelectInput(session, "MethodRef", choices = Methods$MethodRef, selected = MethodRef)
  # })
  #
  observeEvent(input$submit, {

  })
}

#' Launch Shiny app for metadata curation
#'
#' Launch an interface to view and modify metadata of curated data
#'
#' @export
curateR <- function() {
  ui <- curateUI("metadata")
  server <- function(input, output, session) { callModule(curate, "metadata") }
  shinyApp(ui = ui, server = server)
}
