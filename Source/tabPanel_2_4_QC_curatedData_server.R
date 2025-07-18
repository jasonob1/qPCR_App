tabPanel_2_4_QC_curatedData_server <- function(input, output, session, shared){
  # COMPUTE ----
  
  shared$curatedData <- reactive({
    req(shared$filterData())
    
    removeQCgenes <- c(
      input$gdc,
      input$rtc,
      input$ppc
    )
    
    removeQCsamples <- input$ntc
    
    filteredData <- shared$filterData()
    
    filteredData <- filteredData %>%
      select(-all_of(removeQCgenes)) %>%
      filter(!(sample%in%removeQCsamples))
    
    geneCols <- names(filteredData)[names(filteredData)%in%shared$geneList()]
    
    filteredData %>%
      cleanCt(
        target = "NoCt",
        method = input$replaceNoCt,
        geneCols = geneCols,
        group = c("chemical", "dose"), # HARD WIRED - update to be dynamic
        ct_thresh = input$highCt,
        ct_replace = input$replaceCtvalue
      ) %>%
      cleanCt(
        target = "HighCt",
        method = input$replaceHighCt,
        geneCols = geneCols,
        group = c("chemical", "dose"), # HARD WIRED - update to be dynamic
        ct_thresh = input$highCt,
        ct_replace = input$replaceCtvalue
      )
  })
  
  # OUTPUT ----
  output$curatedData <- renderReactable(
    reactable(
      shared$curatedData(),
      defaultColDef = colDef(
        style = function(value) { # to highlight NAs (No Ct) red
          if (is.na(value)) {
            color <- "#dc3815"
          } else {
            color <- "#585858"
          }
          list(color = color, fontWeight = "lighter")
        },
        na="no Ct",
        minWidth = 85,
        align = "center"
      ),
      columns = list(
        sample = colDef(minWidth = 100, align="left"),
        chemical = colDef(align="left")
      ),
      fullWidth = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  )
  
  
  
  # DYNAMICS ----
  
}