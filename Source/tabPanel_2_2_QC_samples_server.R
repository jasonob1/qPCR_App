tabPanel_2_2_QC_samples_server <- function(input, output, session, shared){
  # COMPUTE ----
  
  # NTC Test
  shared$ntcTest <- reactive({
    req(input$ntc, input$ntcThresh)
    
    ntc <- input$ntc
    ntcThresh <- input$ntcThresh
    
    GOIs <- names(shared$filterData())[names(shared$filterData())%in%shared$geneList()]
    
    shared$filterData() %>%
      select(sample, all_of(GOIs)) %>%
      filter(sample%in%ntc) %>%
      mutate(across(all_of(GOIs), 
                    list(
                      TEST = ~.x>ntcThresh | is.na(.x)
                    )
      )) %>%
      select(order(colnames(.))) %>%
      relocate(sample)
  })
  
  # NTC Summary
  shared$ntcSummary <-reactive({
    req(shared$ntcTest())
    
    testTable<-shared$ntcTest()
    
    # Define pass columns
    test_columns <- names(testTable)[grepl("_TEST$", names(testTable))]
    
    # Initialize result data frame
    result <- data.frame(sample=testTable$sample, PassedGenes=NA, FailedGenes=NA)
    
    # Fill Table
    for(i in 1:nrow(result)){
      result$PassedGenes[i] <- testTable %>%
        filter(sample==result$sample[i]) %>%
        select(all_of(test_columns)) %>%
        rowSums()
      result$FailedGenes[i] <- length(test_columns) - result$PassedGenes[i]
    }
    
    return(result)
  })
  
  
  # OUTPUT ----
  
  # NTC Test
  output$ntcTest <- renderReactable({
    req(shared$ntcTest())
    
    testTable <- shared$ntcTest()
    
    ctCols <- names(testTable)[names(testTable)%in%shared$geneList()]
    testCols <- ctCols %>%
      str_c("_TEST")
    
    toRender <- testTable %>%
      mutate(across(all_of(testCols), ~ ifelse(.x, "PASS", "FAIL"))) %>%
      rename_with(~paste0(., "_Ct"), all_of(ctCols))
    
    formatCols <- c(
      str_c(ctCols, "_Ct"),
      testCols
    )
    
    col_defs <- lapply(
      formatCols, function(x){
        colDef(
          style =gdc_style_fun(toRender, x, input$ntcThresh)
        )
      }
    )
    names(col_defs) <- formatCols
    
    col_defs[["sample"]] <- colDef(minWidth = 100, align="left")
    
    reactable(
      toRender,
      defaultColDef = colDef(
        na="no Ct",
        minWidth = 85,
        align = "center"
      ),
      columns = col_defs,
      rowStyle = function(index) {
        if (any(toRender[index, testCols] == "FAIL")) {
          list(background = "#f8e6e6")
        }
      },
      fullWidth = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  })
  
  # NTC Summary
  output$ntcSummary <- renderReactable({
    req(shared$ntcSummary())
    reactable(shared$ntcSummary())
  })
  
  
  # DYNAMIC UI ----
  
  # To High/Low Check Button
  observeEvent(
    input$toLowHigh, {
      updateTabsetPanel(inputId = "qcTab", selected = "Low or High Ct Check")
    })

}