tabPanel_3_3_Norm_normData_server <- function(input, output, session, shared){
  
  # COMPUTE ----
  
  
  # OUTPUT ----
  output$normalizedData <- renderReactable({
    req(shared$normData())
    
    normData <- shared$normData()
    
    GOIs <- names(normData)[names(normData)%in%shared$geneList()]
    
    reactable(
      normData,
      defaultColDef = colDef(
        style = function(value) { # to highlight NAs (No Ct) red
          if (is.na(value)) {
            color <- failCol
          } else {
            color <- passCol
          }
          list(color = color, fontWeight = "lighter")
        },
        na="no Ct",
        minWidth = 85,
        align = "center",
        format = colFormat(digits = 1)
      ),
      columns = list(
        sample = colDef(minWidth = 100, align="left"),
        chemical = colDef(align="left")
      ),
      fullWidth = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  })
  
  # DYNAMICS ----
  
  # DOWNLOAD NORM DATA WHEN AVAILABLE
  observe({
    if(!is.null(shared$normData())){
      output$downloadNormData <- downloadHandler(
        filename = function(){
          "normalizedData.tsv"
        },
        content = function(file){
          write.table(shared$normData(), file, sep="\t", row.names = FALSE)
        }
      )
    } else {
      output$downloadNormData <- NULL
    }
  })
  
  # To DEG Button
  observeEvent(
    input$toDEGs, {
      updateTabsetPanel(inputId = "topTab", selected = "DEG analysis")
    })
  
  
}