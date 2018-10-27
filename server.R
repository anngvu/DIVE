library(shiny)

#-- SET-UP ----------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#

shinyServer(function(input, output, session) {
  
  #-- HOME ----------------------------------------------------------------------------------------#
  
  output$network <- renderVisNetwork({
    g %>% visIgraphLayout(randomSeed = 88) %>% visOptions(highlightNearest = TRUE)
  })
  
  observeEvent(input$accessdata, {
    updateNavbarPage(session, "main", selected = "source-data")
  })
  
  #-- PAGE 1: Matching Module ---------------------------------------------------------------------#
  
  source("moduleM.R", local = T)
  
  #-- PAGE 2: Correlation Module ------------------------------------------------------------------#
  
  source("moduleC.R", local = T)
  
  #-- PAGE 3: Vocalno Module ----------------------------------------------------------------------#
  
  source("moduleV.R", local = T)
  
  #-- PAGE 4: Get Data ----------------------------------------------------------------------------#
  
  output$sourceDT <- DT::renderDataTable({
    show <- Columns[, .(Source, Contributors, IndividualLevelData, MPOLabel, Variable, Description, Relevance, Method, Note, DataSource, DataSourceLink)]
    if(input$filterDT) {
      show <- show[IndividualLevelData == "Yes", ]
    }
    show[IndividualLevelData == "Yes", DataSourceLink := paste0("<a href='",DataSourceLink,"' target='_blank'"," title='",DataSourceLink,"'>Get from original source","</a>")]
    show[IndividualLevelData != "Yes", DataSourceLink := ""]
    setnames(show, c("MPOLabel", "IndividualLevelData", "DataSource", "DataSourceLink"), 
             c("Theme or Area of Interest", "Individual-Level Data", "Source", "Data Source Link"))
  }, escape = FALSE, rownames = F, options = list(dom = 'ftp', pageLength = 7))

  output$downloadCollection <- downloadHandler(
    filename = function() {
      "Archive.zip"
    },
    content = function(file) {
      file.copy("Collection/Archive.zip", file)
    },
    contentType = "application/zip"
  )
  
})
