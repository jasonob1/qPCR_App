tabPanel_3_1_Norm_hkGenes_server <- function(input, output, session, shared){
  
  # COMPUTE ----
  
  # Curated gene list
  shared$curGeneList <- reactive({
    req(shared$curatedData(), shared$geneList())
    names(shared$curatedData())[names(shared$curatedData()) %in% shared$geneList()] 
  })
  
  # hk Gene ANOVA
  shared$hkANOVA <- reactive({
    req(shared$curatedData(), input$hkg, input$hkFactor)
    
    curatedData <- shared$curatedData()
    hkg <- input$hkg
    aovFactor <- input$hkFactor
    
    hkANOVA <- curatedData %>%
      hkTest(hkNames=hkg, aovFactor=aovFactor)
    
    return(hkANOVA)
  })
  
  
  ### TO DO:
  #  results summary text
  #  result table formatting
  
  
  
  # OUTPUT ----
  
  # hkANOVA summary
  output$hkANOVA <- renderReactable({
    req(shared$hkANOVA())
    
    resList<-shared$hkANOVA()
    
    resTable <- data.frame(
      Gene = names(resList),
      ANOVA_p_value = sapply(resList, function(x) x$AOV),
      row.names = NULL
    )
    
    resTable$Result <- ifelse(resTable$ANOVA_p_value > 0.05, "PASS", "FAIL")
    
    reactable(
      resTable %>%
        mutate(ANOVA_p_value = sprintf("%.3f",ANOVA_p_value))
    )
    
  })
  
  # hkANOVA multiple comparison
  output$hkMultiComp <- renderReactable({
    req(shared$hkANOVA())
    
    resList <- shared$hkANOVA()
    
    resTable <-do.call(
      rbind,
      lapply(
        names(resList),
        function(gene){
          df <- resList[[gene]]$variable_specific
          df <- df[rownames(df) != "(Intercept)", , drop=FALSE]
          data.frame(
            Gene = gene,
            Level = rownames(df),
            p_value = df$p_value,
            row.names = NULL
          )
        }
      )
    )
    
    resTable$Result <- ifelse(resTable$p_value > 0.05, "PASS", "FAIL")
    
    reactable(
      resTable %>%
        mutate(p_value = sprintf("%.3f",p_value)),
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10,25,50,100)
    )
  
  })
  
  # hkGene PLots
  output$hkPlot <- renderPlot({
    req(shared$curatedData(), input$hkg, input$hkFactor)
    
    plotData <- shared$curatedData()
    hkg <- input$hkg
    hkFactor <- input$hkFactor
    
    axis.title.size<-18
    axis.text.size<-16
    
    y_lims<-plotData %>%
      select(all_of(hkg)) %>%
      range()
    y_lims<-y_lims + c(-1,1)
    
    plots<-lapply(hkg, function(GENE){
      if(length(hkFactor)<2){   ## keeping this for now to allow multiple factors in the future
        plotData %>%
          ggplot(aes(x=.data[[hkFactor]], y=.data[[GENE]])) +
          ylim(y_lims) +
          geom_jitter(size=3, position=position_dodge2(width=0.4)) +
          theme_bw() +
          theme(
            axis.title = element_text(size = axis.title.size),
            axis.text = element_text(size = axis.text.size)
          )
      }else{
        plotData %>%
          ggplot(aes(x=.data[[hkFactor]], y=.data[[GENE]], colour = .data[[aovFactor]])) +
          ylim(y_lims) +
          geom_jitter(size=3, position=position_dodge2(width=0.4)) +
          theme_bw() +
          theme(
            axis.title = element_text(size = axis.title.size),
            axis.text = element_text(size = axis.text.size)
          )
      }
    })
    ggarrange(plotlist = plots, nrow=length(hkg))  
    
    
  })
  
  
  
  
  # DYNAMICS ----
  
  # update selectInput with curated gene list
  observeEvent(shared$curGeneList(),{
    curGeneChoice <- c(shared$curGeneList())
    updateSelectInput(inputId = "hkg", choices = curGeneChoice)
  })
  
}