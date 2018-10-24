fluidPage(
fluidRow(class = "top-options",
         column(1,
                br(),
                actionButton("guideVolcano", "Guide", icon = icon("info-circle")),
                br(), br(),
                actionButton("viewVolcano", "", icon = icon("eye-slash")),
                actionButton("resetVolcano", "Reset", icon = icon("undo"))
         ),       
         column(3,
                div(class = "forceInline", selectizeInput("Glist", "Genes (proteins) of interest", 
                                                          choices = NULL, selected = NULL, 
                                                          options = list(maxItems = 50)), width = "375px"),
                div(class = "forceInline", br(), actionButton("highlight", "", icon = icon("check"))),
                div(class = "forceInline", br(), actionButton("gSets", "", icon = icon("list"))),
                helpText("Note: Expression values may not be available in all datasets.")
         ),
         column(2, align="right", 
                radioButtons("GorR", "Look up genes (proteins) involved in a process/pathway with:", 
                             choices = c("Gene Ontology", "Reactome"),
                             selected = "Gene Ontology", inline = T),
                conditionalPanel(condition = "input.GorR == 'Gene Ontology'",
                                 radioButtons("BPCCMP", "GO branch",
                                              choices = c("BP" = "BP", "CC" = "CC", "MF" = "MF"),
                                              selected = "BP", inline = T))
         ),
         column(3,
                div(class = "forceInline", selectizeInput("GOReactq", "Choose term", 
                                                          choices = NULL, selected = NULL, width = "240px")),
                div(class = "forceInline", br(), actionButton("Ont", "", icon = icon("check"))),
                helpText("Only terms with at least one annotation shown.")
         ),
         column(3, style="border-left: 1px solid lightgray",
                div(class = "forceInline", selectizeInput("Clist", "Phenotype/clinical variable(s)", 
                                                          choices = Columns[Source %in% c("Aab", "Demographics", "DiabetesInfo", "HLARisk"), Variable], 
                                                          selected = character(0), multiple = T, 
                                                          options = list(placeholder = "select genes first", maxItems = 3), width = "240px")),
                div(class = "forceInline", br(), actionButton("clistAdd", "", icon = icon("check"))),
                helpText("See gene/protein expression values relative to phenotype/clinical data.")
         )),
fluidRow(
  column(12,
         conditionalPanel("input.viewVolcano%2==1",
                          checkboxGroupInput("activeVolcano", "Show/hide volcano plots:", 
                                             choiceNames = c("Transcriptomics | AAB-HC (Yip et et. unpublished)", 
                                                             "Transcriptomics | T1D-HC (Yip et et. unpublished)", 
                                                             "Proteomics | T1D-HC (Liu et al. 2016)", 
                                                             "Proteomics | T1D-HC (Nyalwidhe et al. 2017)", 
                                                             "Proteomics | AAB-HC (Nyawidle et al. 2016)"),
                                             choiceValues = c("gx.AAB", "gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                             selected = c("gx.T1D", "px1", "px2.T1D", "px2.AAB"),
                                             inline = T)
         )
  )),
fluidRow(
  column(12,
         withSpinner(plotlyOutput("volcanos"), type = 4, color = "gray")
  )),
fluidRow(
  column(12, style="margin-top: 50px;",
         radioButtons("whichX", "Dataset", 
                      choices = c("Transcriptomics | Pancreas" = "gx", "Proteomics | Exocrine" = "px1", "Proteomics | Endocrine" = "px2"), 
                      selected = "gx", inline = T)
  ),
  column(12,
         plotlyOutput("parallel")
  ))
)