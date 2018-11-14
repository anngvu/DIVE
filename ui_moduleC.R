fluidPage(
  fluidRow(
    column(1,
           br(),
           actionButton("guideCorrelation", "Guide", icon = icon("info-circle"))
    ),
    column(7, div(id = "corrFilters",
                  div(class = "forceInline", numericInput("minimumN", HTML("min. N for <i>r</i>"), min = 2, max = NA, step = 1, val = 5, width = "80px")),
                  HTML("&nbsp"),
                  div(class = "forceInline", selectInput("varMenuOpt", "Filter variables by", 
                                                         choices = c(`name` = "variable", `cell/tissue origin` = "cell", `publication source` = "author"), 
                                                         width = "170px")),
                  div(class = "forceInline", selectizeInput("varMenu", icon("search"), colnames(cor.data$corM), multiple = T, 
                                                            options= list(placeholder = "..."), width = "500px")),
                  div(id = "EKR", class = "forceInline",
                     div(class = "forceInline", br(), actionButton("varKeep", icon("filter"))),
                     div(class = "forceInline", br(), actionButton("varReset", "Reset", icon = icon("undo")))
                  ),
                  conditionalPanel("input.varMenuOpt == 'cell'", 
                                   absolutePanel(id = "cellpackpanel", width = "450px", height = "550px", draggable = T, left = 400,
                                                 h4("A map of data by cell/tissue origin"), 
                                                 HTML("Cells are shown within larger circles that represent particular tissue contexts.  
                                                 This is most relevant for <span style='font-weight:bold; color:dodgerblue'>immune cells</span>, which can be assayed in different tissues,
                                                 e.g. <span style='background-color:lemonchiffon'>pancreatic</span>, <span style='background-color:lightcoral'>hemolymphoid</span>,
                                                 <span style='background-color:rosybrown'>intestinal</span>, unlike <span style='font-weight:bold; color:deeppink'>pancreas cells</span>. Hover for info, click to zoom, double-click to select."),
                                                 d3Output("cellpack", width = "400px", height = "400px")))
    )),
  column(4,
         div(id = "corrUpload",
             div(class = "forceInline",
                 fileInput("dataUpload", "", multiple = FALSE, width = "300px", 
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
                           buttonLabel = "My data", placeholder = "Upload to compare..")),
             div(class = "forceInline",
                 br(), actionButton("helpUpload", "", icon = icon("question-circle"), width = "10px"))
         )
  )),
  fluidRow(
    column(8, align = "left",
           div(id = "corM", style ="height: 1000px;", 
               plotlyOutput("corM"))
    ),
    column(4,
           div(id = "drilldown", style = "margin-top: 20px; margin-left: 10px;",
               selectizeInput("drilldown", "Drill down to data for", 
                              choices = c("", colnames(cor.data$corM)), selected = "", 
                              options = list(maxItems = 2, placeholder = "select variable(s)"), width = "400px"),    
               conditionalPanel("input.drilldown",
                                div(class = "forceInline", 
                                    selectInput("colorby", "Color data points by", 
                                                choices = names(cdata)[!names(cdata) %in% "ID"], 
                                                selected = "donor.type", width = "200px")),
                                HTML("&nbsp"),
                                div(class = "forceInline", br(), actionButton("switchXY", "XY", icon = icon("refresh"))),
                                HTML("&nbsp"),
                                div(class = "forceInline", br(), checkboxInput("plotsmooth", "Add smooth")),
                                plotlyOutput("scatter")
               )
           )
    ))
)

