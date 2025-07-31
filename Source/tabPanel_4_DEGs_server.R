tabPanel_4_DEGs_server <- function(input, output, session, shared){
  # COMPUTE ----
  
  # List of treatment groups in selected Factor (in order to allow user to select control group)
  shared$fcFactorGroups <- reactive({
    req(shared$normData(), input$fcFactor)
    
    normData <- shared$normData()
    
    fcFactorGroups <- normData %>%
      pull(all_of(input$fcFactor)) %>%
      unlist() %>%
      unique()
  })
  
  # Fold Change Results
  shared$fcResults <- reactive({
    req(shared$normData(), input$fcFactor, input$fcControlGroup)
    
    normData <- shared$normData()
    GOIs <- names(normData)[names(normData)%in%shared$geneList()]
    fcFactor <- input$fcFactor
    fcControlGroup <- input$fcControlGroup
    
    fcResults <- normData %>%
      mutate(across(all_of(GOIs), ~2^-(.) )) %>%
      mutate(across(all_of(GOIs), ~./geoMean(.[!!sym(fcFactor)%in%fcControlGroup])))
  })
  
  # OUTPUT ----
  output$foldChangeTable <- renderReactable({
    req(shared$fcResults())
    
    fcResults <- shared$fcResults()
    
    reactable(
      fcResults,
      defaultColDef = colDef(
        minWidth = 85,
        align = "center",
        format = colFormat(digits = 2)
      )
    )
  })
  
  # DYNAMICS ----
  
  # update selectInput with for Control Sample List
  observeEvent(shared$fcFactorGroups(),{
    fcGroupChoices <- c(shared$fcFactorGroups())
    updateSelectInput(inputId = "fcControlGroup", choices = fcGroupChoices)
  })
  
}