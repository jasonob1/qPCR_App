tabPanel_2_3_QC_lowHighCt_server <- function(input, output, session, shared){
  
  # COMPUTE ----
  
  # lowHighCt Test
  shared$lowHighCtTest <- reactive({
    req(input$lowCt, input$highCt, input$lowHighPerc, shared$curatedData())
    
    curatedData <- shared$curatedData()
    
    GOIs <- names(curatedData)[names(curatedData)%in%shared$geneList()]
    
    curatedData %>%
      select(sample, all_of(GOIs)) %>%
      mutate(across(all_of(GOIs),
                    list(
                      TEST = ~case_when(
                        .x < input$lowCt ~ "lowCt",
                        .x > input$highCt | is.na(.x) ~ "highCt",
                        TRUE ~ "PASS"
                      )
                    )
      )) %>%
      select(order(colnames(.))) %>%
      relocate(sample)
    
  })
  
  # lowHighCtGene Summary
  shared$lowHighCtGeneSummary <- reactive({
    req(shared$lowHighCtTest())
    
    testTable <- shared$lowHighCtTest()
    
    ctCols <- names(testTable)[names(testTable)%in%shared$geneList()]
    testCols <- ctCols %>%
      str_c("_TEST")
    
    testTable %>%
      select(all_of(testCols)) %>%
      summarise(
        across(
          everything(),
          list(
            countLowCt = ~sum(.x == "lowCt"),
            percentLowCt = ~round(mean(.x == "lowCt")*100,1),
            countHighCt = ~sum(.x == "highCt"),
            percentHighCt = ~round(mean(.x == "highCt")*100,1)
          ),
          .names = "{.col}_{.fn}"
        )
      ) %>%
      pivot_longer(
        everything(),
        names_to = c("gene", ".value"),
        names_sep = "_TEST_"
      ) %>%
      filter(countLowCt > 0 | countHighCt > 0)
  })
  
  # lowHighCtGene Summary Text
  shared$lowHighCtGeneSumText <- reactive({
    req(shared$lowHighCtGeneSummary())
    
    testRows <- nrow(shared$lowHighCtGeneSummary())
    
    sumText <- if_else(testRows>0,
                       "Genes with a high precentage of poor Cts are shown below. Consider removing these genes from your analysis (on 'Upload Data' tab)",
                       "All of your genes passed!"
                       )
  })
  
  # lowHighCt Sample Summary
  shared$lowHighCtSampleSummary <- reactive({
    req(shared$lowHighCtTest())
    
    testTable <- shared$lowHighCtTest()
    
    ctCols <- names(testTable)[names(testTable)%in%shared$geneList()]
    testCols <- ctCols %>%
      str_c("_TEST")
    
    testTable %>%
      select(sample, all_of(testCols)) %>%
      rowwise() %>%
      mutate(
        countLowCt = sum(c_across(all_of(testCols)) == "lowCt"),
        percentLowCt = round(
          mean(c_across(all_of(testCols)) == "lowCt") * 100, 1
        ),
        countHighCt = sum(c_across(all_of(testCols)) == "highCt"),
        percentHighCt = round(
          mean(c_across(all_of(testCols)) == "highCt") * 100, 1
        )
      ) %>%
      ungroup() %>%
      select(-all_of(testCols)) %>%
      filter(percentLowCt >= input$lowHighPerc | percentHighCt >= input$lowHighPerc)
    
  })
  
  # lowHighCtSample Summary Text
  shared$lowHighCtSampleSumText <- reactive({
    req(shared$lowHighCtSampleSummary())
    
    testRows <- nrow(shared$lowHighCtSampleSummary())
    
    sumText <- if_else(testRows>0,
                       "Samples with a high precentage of poor Cts are shown below. Consider removing these samples from your analysis (on 'Upload Data' tab)",
                       "All of your samples passed!"
    )
  })
  
  # NOTE: Ct replacement computations are done in the "CuratedData" server souce code
  
  
  # OUTPUT ----
  
  # lowHighCt Test
  output$lowHighCtTest <- renderReactable({
    req(shared$lowHighCtTest(), shared$lowHighCtGeneSummary())
    
    testTable <- shared$lowHighCtTest()
    
    ### TO DO: HIGHLIGHT FAILED COLUMNS (failCols) IN COL STYLE FUNCTION (or other means)?
    # failCols <- shared$lowHighCtGeneSummary() %>%
    #   filter(percentLowCt > input$lowHighPerc | percentHighCt > input$lowHighPerc) %>%
    #   pull(sample) %>%
    #   unlist()
    # this doesnt work yet. But not urgent

    ctCols <- names(testTable)[names(testTable)%in%shared$geneList()]
    testCols <- ctCols %>%
      str_c("_TEST")
    formatCols <-c(ctCols, testCols)
    
    col_defs <-lapply(
      formatCols, function(x){
        colDef(
          style=lowHighCt_style_fun(
            testTable,
            x,
            input$lowCt,
            input$highCt)
        )
      }
    )
    names(col_defs) <- formatCols
    
    reactable(
      testTable,
      defaultColDef = colDef(
        na="no Ct",
        minWidth = 90,
        align="center"
      ),
      columns = col_defs,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10,25,50,100)
              
    )
  })
  
  # lowHighCt Gene Summary
  output$lowHighCtGeneSummary <- renderReactable({
    req(shared$lowHighCtGeneSummary())
    
    summaryTable <- shared$lowHighCtGeneSummary()
    
    reactable(summaryTable,
      defaultColDef = colDef(
        minWidth = 60,
        align="center"
      ),
      columns = list(
        gene = colDef(align="left")
      ),
      rowStyle = function(index){
        if(any(summaryTable[index, "percentLowCt"] > input$lowHighPerc | summaryTable[index, "percentHighCt"] > input$lowHighPerc )){
          list(
            background = failBack,
            color = failCol,
            fontWeight = failFont
          )
        }
      },
      fullWidth = TRUE
    )
  })
  
  #lowHighCt Gene Summary Text
  output$lowHighCtGeneSumText <- renderText({
    shared$lowHighCtGeneSumText()
  })
  
  # lowHighCt Sample Summary
  output$lowHighCtSampleSummary <-renderReactable({
    req(shared$lowHighCtSampleSummary())
    
    summaryTable <- shared$lowHighCtSampleSummary()
    
    reactable(summaryTable)
  })
  
  #lowHighCt Sample Summary Text
  output$lowHighCtSampleSumText <- renderText({
    shared$lowHighCtSampleSumText()
  })
  
  
  # DYNAMIC UI ----
  
  # To CuratedData Button
  observeEvent(
    input$toCuratedData, {
      updateTabsetPanel(inputId = "qcTab", selected = "Curated Data")
    })
  
}