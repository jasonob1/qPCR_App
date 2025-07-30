tabPanel_3_2_Norm_normalization_server <- function(input, output, session, shared){
  
  # COMPUTE ----
  shared$normData <- reactive({
    req(shared$curatedData(), input$normMethod)
    
    curatedData <- shared$curatedData()
    GOIs <- names(curatedData)[names(curatedData)%in%shared$geneList()]
    
    if(input$normMethod == "house keeping genes"){
      
      # House Keeping Gene Normalization
      hkg <- input$hkg
      
      hkNormFactor <- curatedData %>%
        dplyr::select(all_of(hkg)) %>%
        rowMeans()
      
      dctData <- curatedData %>%
        mutate(across(all_of(GOIs), ~(.-hkNormFactor))) %>%
        dplyr::select(-all_of(hkg))
      
      normData <- dctData
      
    } else if(input$normMethod == "Trimmed mean of M values (TMM)"){
      
      # TMM Normalization
      
      logratioTrim <- 0.3
      sumTrim <- 0.05
      
      testData <- curatedData %>%
        select(all_of(GOIs)) %>%
        mutate(across(all_of(GOIs), ~2^-.)) %>% # transform to "relative expression" (i.e. 2^-Ct)
        t() %>% # edgeR requires samples in cols and genes in rows
        as.data.frame()
      
      names(testData) <- curatedData$sample
      
      sizeFactor<- calcNormFactors(
        testData,
        method = "TMM",
        logratioTrim = logratioTrim,
        sumTrim = sumTrim
      )
      
      tmmData <- testData
      for(i in 1:ncol(tmmData)){
        tmmData[,i]<-tmmData[,i]/(sum(tmmData[,i])*sizeFactor[i])
      }
      tmmData <- tmmData %>%
        t() %>% # transform back to original structure
        as_tibble()
      
      normData <- curatedData %>%
        dplyr::select(-all_of(GOIs)) %>%
        bind_cols(tmmData) %>%
        mutate(across(all_of(GOIs), ~-log(.,2))) # transform back to logarithmic scale
      
      return(normData)
      
    } else if(input$normMethod == "none"){
      normData <- curatedData
    }
    
    return(normData)
  })
  
  # OUTPUT ----
  
  # Pre normalized plot 
  output$preNormDist <- renderPlot({
    req(shared$curatedData(), input$normMethod)
    
    plotData <- shared$curatedData()
    
    if(input$normMethod == "house keeping genes"){
      plotData <- plotData %>%
        select(-any_of(input$hkg))
    }
    
    GOIs <- names(plotData)[names(plotData)%in%shared$geneList()]
    sampleNames <- plotData %>%
      pull(sample) %>%
      unlist()
    
    plotData <- plotData %>%
      select(all_of(GOIs)) %>%
      as.data.frame()
    
    row.names(plotData) <- sampleNames
    
    longData <- plotData %>%
      t %>%
      data.frame() %>%
      stack() %>%
      setNames(c("ct","sample"))
    
    ggplot(longData, aes(x=sample, y=ct)) +
      geom_boxplot(outlier.size = 0.5) +
      theme_minimal(base_size=16) +
      theme(
        axis.text.x = element_text(angle=90, hjust=1, size=12)
      )
  })
  
  
  # Normalized Plot
  output$normDist <- renderPlot({
    req(shared$normData())
    
    plotData <- shared$normData()
    
    GOIs <- names(plotData)[names(plotData)%in%shared$geneList()]
    sampleNames <- plotData %>%
      pull(sample) %>%
      unlist()
    
    plotData <- plotData %>%
      select(all_of(GOIs)) %>%
      as.data.frame()
    
    row.names(plotData) <- sampleNames
    
    longData <- plotData %>%
      t %>%
      data.frame() %>%
      stack() %>%
      setNames(c("ct","sample"))
    
    ggplot(longData, aes(x=sample, y=ct)) +
      geom_boxplot(outlier.size = 0.5) +
      theme_minimal(base_size=16) +
      theme(
        axis.text.x = element_text(angle=90, hjust=1, size=12)
      )
  })
  
  
  # DYNAMICS ----
  # To Normalization Button
  observeEvent(
    input$toNormData, {
      updateTabsetPanel(inputId = "normTab", selected = "Normalized Data")
    })
  
  
  
}