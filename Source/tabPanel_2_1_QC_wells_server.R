tabPanel_2_1_QC_wells_server <- function(input, output, session, shared){
  
  # COMPUTE ----
  
  # GDC Test
  shared$gdcTest <- reactive({
    req(input$gdc, input$gdcThresh)
    
    gdc <- input$gdc
    gdcThresh <- input$gdcThresh
    
    shared$filterData() %>%
      select(sample, all_of(gdc)) %>%
      mutate(across(all_of(gdc), 
                    list(
                      TEST = ~.x>gdcThresh | is.na(.x)
                    )
      )) %>%
      select(order(colnames(.))) %>%
      relocate(sample)
  })
  
  # GDC Summary
  shared$gdcSummary <-reactive({
    req(shared$gdcTest())
    
    # Define pass columns
    test_columns <- names(shared$gdcTest())[grepl("_TEST$", names(shared$gdcTest()))]
    
    # Initialize result data frame
    outcomes <- c(TRUE, FALSE)
    result <- data.frame(outcome = outcomes)
    
    # Loop through each pass column and add counts
    for(col in test_columns) {
      counts <- table(shared$gdcTest()[[col]])
      
      # Make sure both TRUE and FALSE are represented
      result[[col]] <- 0  # Initialize with zeros
      
      # Fill in actual counts where available
      for(outcome in names(counts)) {
        idx <- which(result$outcome == as.logical(outcome))
        result[idx, col] <- counts[outcome]
      }
    }
    return(result)
  })
  
  # RTC test
  shared$rtcTest <- reactive({
    req(input$rtc, input$rtcThresh)
    
    rtc <- input$rtc
    rtcThresh <- input$rtcThresh
    
    shared$filterData() %>%
      select(sample, all_of(rtc)) %>%
      mutate(across(all_of(rtc), 
                    list(
                      TEST = ~.x<rtcThresh & !is.na(.x)
                    )
      )) %>%
      select(order(colnames(.))) %>%
      relocate(sample)
  })
  
  # RTC Summary
  shared$rtcSummary <-reactive({
    req(shared$rtcTest())
    
    # Define pass columns
    test_columns <- names(shared$rtcTest())[grepl("_TEST$", names(shared$rtcTest()))]
    
    # Initialize result data frame
    outcomes <- c(TRUE, FALSE)
    result <- data.frame(outcome = outcomes)
    
    # Loop through each pass column and add counts
    for(col in test_columns) {
      counts <- table(shared$rtcTest()[[col]])
      
      # Make sure both TRUE and FALSE are represented
      result[[col]] <- 0  # Initialize with zeros
      
      # Fill in actual counts where available
      for(outcome in names(counts)) {
        idx <- which(result$outcome == as.logical(outcome))
        result[idx, col] <- counts[outcome]
      }
    }
    return(result)
  })
  
  # PPC test
  shared$ppcTest <- reactive({
    req(input$ppc, input$ppcThresh)
    
    ppc <- input$ppc
    ppcThresh <- input$ppcThresh
    
    shared$filterData() %>%
      select(sample, all_of(ppc)) %>%
      mutate(across(all_of(ppc), 
                    list(
                      TEST = ~.x<ppcThresh & !is.na(.x)
                    )
      )) %>%
      select(order(colnames(.))) %>%
      relocate(sample)
  })
  
  # PPC Summary
  shared$ppcSummary <-reactive({
    req(shared$ppcTest())
    
    # Define pass columns
    test_columns <- names(shared$ppcTest())[grepl("_TEST$", names(shared$ppcTest()))]
    
    # Initialize result data frame
    outcomes <- c(TRUE, FALSE)
    result <- data.frame(outcome = outcomes)
    
    # Loop through each pass column and add counts
    for(col in test_columns) {
      counts <- table(shared$ppcTest()[[col]])
      
      # Make sure both TRUE and FALSE are represented
      result[[col]] <- 0  # Initialize with zeros
      
      # Fill in actual counts where available
      for(outcome in names(counts)) {
        idx <- which(result$outcome == as.logical(outcome))
        result[idx, col] <- counts[outcome]
      }
    }
    return(result)
  })
  
  
  # OUTPUT ----
  
  # GDC Test
  output$gdcTest <- renderReactable({
    req(shared$gdcTest())
    
    ctCols <- input$gdc
    testCols <- ctCols %>%
      str_c("_TEST")
    
    toRender <- shared$gdcTest() %>%
      mutate(across(all_of(testCols), ~ ifelse(.x, "PASS", "FAIL"))) %>%
      rename_with(~paste0(., "_Ct"), all_of(ctCols))
    
    formatCols <- c(
      str_c(ctCols, "_Ct"),
      testCols
    )
    
    col_defs <- lapply(
      formatCols, function(x){
        colDef(
          style =gdc_style_fun(toRender, x, input$gdcThresh)
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
          list(background = failBack)
        }
      },
      fullWidth = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  })
  
  # GDC Summary
  output$gdcSummary <- renderReactable({
    req(shared$gdcSummary())
    toRender <- shared$gdcSummary() %>%
      #mutate(outcome = as.character(outcome)) %>%
      mutate(outcome = case_when(
        outcome == TRUE ~ "PASS",
        outcome == FALSE ~ "FAIL"
      ))
    
    names(toRender) <- str_replace(names(toRender), "_TEST", "")
    
    reactable(
      toRender,
      defaultColDef = colDef(
        minWidth = 60,
        align = "center"
      ),
      columns = list(
        outcome = colDef(align="left")
      ),
      rowStyle = function(index) {
        if (any(toRender[index, "outcome"] == "FAIL")) {
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
  
  # RTC Test
  output$rtcTest <- renderReactable({
    req(shared$rtcTest())
    
    ctCols <- input$rtc
    testCols <- ctCols %>%
      str_c("_TEST")
    
    toRender <- shared$rtcTest() %>%
      mutate(across(all_of(testCols), ~ ifelse(.x, "PASS", "FAIL"))) %>%
      rename_with(~paste0(., "_Ct"), all_of(ctCols))
    
    formatCols <- c(
      str_c(input$rtc, "_Ct"),
      testCols
    )
    
    col_defs <- lapply(
      formatCols, function(x){
        colDef(
          style =rtc_ppc_style_fun(toRender, x, input$rtcThresh)
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
          list(background = failBack)
        }
      },
      fullWidth = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  })
  
  #RTC Summary
  output$rtcSummary <- renderReactable({
    req(shared$rtcSummary())
    toRender <- shared$rtcSummary() %>%
      #mutate(outcome = as.character(outcome)) %>%
      mutate(outcome = case_when(
        outcome == TRUE ~ "PASS",
        outcome == FALSE ~ "FAIL"
      ))
    
    names(toRender) <- str_replace(names(toRender), "_TEST", "")
    
    reactable(
      toRender,
      defaultColDef = colDef(
        minWidth = 60,
        align = "center"
      ),
      columns = list(
        outcome = colDef(align="left")
      ),
      rowStyle = function(index) {
        if (any(toRender[index, "outcome"] == "FAIL")) {
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
  
  # PPC Test
  output$ppcTest <- renderReactable({
    req(shared$ppcTest())
    
    ctCols <- input$ppc
    testCols <- ctCols %>%
      str_c("_TEST")
    
    toRender <- shared$ppcTest() %>%
      mutate(across(all_of(testCols), ~ ifelse(.x, "PASS", "FAIL"))) %>%
      rename_with(~paste0(., "_Ct"), all_of(ctCols))
    
    formatCols <- c(
      str_c(input$ppc, "_Ct"),
      testCols
    )
    
    col_defs <- lapply(
      formatCols, function(x){
        colDef(
          style =rtc_ppc_style_fun(toRender, x, input$ppcThresh)
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
          list(background = failBack)
        }
      },
      fullWidth = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  })
  
  #PPC Summary
  output$ppcSummary <- renderReactable({
    req(shared$ppcSummary())
    toRender <- shared$ppcSummary() %>%
      #mutate(outcome = as.character(outcome)) %>%
      mutate(outcome = case_when(
        outcome == TRUE ~ "PASS",
        outcome == FALSE ~ "FAIL"
      ))
    
    names(toRender) <- str_replace(names(toRender), "_TEST", "")
    
    reactable(
      toRender,
      defaultColDef = colDef(
        minWidth = 60,
        align = "center"
      ),
      columns = list(
        outcome = colDef(align="left")
      ),
      rowStyle = function(index) {
        if (any(toRender[index, "outcome"] == "FAIL")) {
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
  
  
  # DYNAMIC UI ----
  
  # To QC Sample Button
  observeEvent(
    input$toQCsamp, {
      updateTabsetPanel(inputId = "qcTab", selected = "QC Samples")
    })
  
}