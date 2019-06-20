# Gives metadata fields in metadata
metadata_fields <- function() {

}

curateUI <- function(id, metadata) {

  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("paper"),
    fluidRow(radioButtons(ns("section"), "Section", choices = c("Availability", "Main annotation", "Other annotation", "Metrics"))),
    fluidRow(
      # Basic data fields
      div(class = "forceInline", textInput(ns("SourceType"), "Source Type")), # nPOD Core vs. Independent Experiment?
      div(class = "forceInline", textInput(ns("Publication"), "Source Publication")),
      div(class = "forceInline", textInput(ns("Reference"), "Reference")), # as in Li et al. 2009; calculate in the backend?
      # div(class = "forceInline", textInput(ns("People"), "People")), # this is calculated in the backend and allows searching by person, not just publication
      # div(class = "forceInline", textInput(ns("VarID"), "Variable ID")), # this is assigned in the backend
      div(class = "forceInline", textInput(ns("Variable"), "Variable Name")),
      div(class = "forceInline", selectInput(ns("Value"), "Variable Value Type", choices = "", options = list(create = T))), # Should be associated with Method
      div(class = "forceInline", textInput(ns("Description"), "Variable Description")),
      div(class = "forceInline", numericInput(ns("Dimensions"), "Dimensions", value = 1, min = 1)),

      # Data availability and lineage; re-derived/transformed/summarized/etc. data has a parent
      conditionalPanel(condition = "input.section == 'Availability'", ns = ns,
        div(class = "forceInline", selectInput(ns("IndividualLevelData"), "Individual-level Data"), choices = c("Yes", "No", "Unknown")),
        div(class = "forceInline", selectInput(ns("Shared"), "Shared", c("Yes", "No/Contact author"))),
        div(class = "forceInline", selectInput(ns("Derived"), "Derived From", choices = ""))
        # Data dividends -- How data is derived should be in methods -- data parent goes here
        # Derivation method should be in methods
      ),

      # Technical curator annotations -- ontology-based, coded values
      conditionalPanel(condition = "input.section == 'Main annotation'", ns = ns,
        div(class = "forceInline", selectInput(ns("Level"), "Level")),
        div(class = "forceInline", selectInput(ns("CellTissue"), "Cell or Tissue"), choices = O$CL$Name),
        div(class = "forceInline", selectInput(ns("TissueContext"), "Tissue Context", choices = O$CL$Name)),
        div(class = "forceInline", selectizeInput(ns("EFO"), "EFO Annotation", choices = "")),
        div(class = "forceInline", selectizeInput(ns("GO"), "GO Process Annotation", choices = O$GO$Name)),
        div(class = "forceInline", selectizeInput(ns("Entrez"), "Entrez Gene IDs", choices = "")),
        div(class = "forceInline", fileInput(ns("Entrez_file"), "Entrez Gene IDs")),
        # Methods
        div(class = "forceInline", selectInput(ns("MethodRef"), "Method Reference", choices = Methods$MethodRef)),
        div(class = "forceInline", actionButton(ns("addMethod"), "Method")), # shows input fields for adding new method
        div(class = "forceInline", uiOutput(ns("inputMethod"), inline = T))
      ),

      # Thematic curator+researcher annotation -- coded values, free text
      conditionalPanel(condition = "input.section == 'Other annotation'", ns = ns,
        div(class = "forceInline", textInput(ns("Tags"), "Tags")),
        div(class = "forceInline", textInput(ns("VarWithinD"), "Variance within Disease")),
        div(class = "forceInline", textInput(ns("VarDvsCtrl"), "Variance Disease vs Control")),
        div(class = "forceInline", textInput(ns("Relevance"), "Relevance Summary")),
        div(class = "forceInline", textInput(ns("PrivateNote"), "PrivateNote")),
        div(class = "forceInline", textInput(ns("PublicNote"), "Note"))
      ),

      # Additional tracking and metrics
      conditionalPanel(condition = "input.section == 'Metrics'", ns = ns,
        div(class = "forceInline", textInput(ns("LastReviewed"), "Last Reviewed")),
        div(class = "forceInline", textInput(ns("DataSource"), "Source Figure or Table")),
        div(class = "forceInline", textInput(ns("DataSourceLink"), "Source Link")),
        div(class = "forceInline", textInput(ns("Interest"), "Interest")) # calculated from downloads + browsers have clicked "I would like to see more data similar to this"
      )
    ),
    fluidRow(
      actionButton(ns("submit"), "Submit")
    )
  )
}

curate <- function(input, output, session,
                   metadata = metadata()) {

  output$inputMethod <- renderUI({
    input$addMethod
    tagList(
      div(class = "forceInline", selectInput(session$ns("Method"), "Method", choices = O[["OBI"]][["Name"]])),
      div(class = "forceInline", textInput(session$ns("MethodDetails"), "Method Details")),
      div(class = "forceInline", actionButton(session$ns("saveMethod"), "Save"))
    )
  })

  observeEvent(input$saveMethod, {
    # create MethodRef and save into Methods file
    MethodRef <- paste0(input$Reference, "::", input$Method)
    new <- list(MethodRef = MethodRef, Method = input$Method, MethodDetails = input$MethodDetails)
    Methods <<- rbind(Methods, new)
    updateSelectInput(session, "MethodRef", choices = Methods$MethodRef, selected = MethodRef)
  })

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
