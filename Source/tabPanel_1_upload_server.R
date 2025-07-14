tabPanel_1_upload_server <- function(input, output, session, shared){
  
  # COMPUTE ----
  shared$longData <- reactive({
    req(input$layoutFile, input$qPCRFiles)
    
    # import layouts
    wellLayout <- read_excel(input$layoutFile$datapath, sheet = "Well Layout") %>%
      column_to_rownames("well") %>%
      pivot_longer(cols = everything(), names_to = NULL)
    
    geneLayout <- read_excel(input$layoutFile$datapath, sheet = "Gene Layout") %>%
      column_to_rownames("well") %>%
      pivot_longer(cols = everything(), names_to = NULL)
    
    sampleLayout <- read_excel(input$layoutFile$datapath, sheet = "Sample Layout") %>%
      column_to_rownames("well") %>%
      # need to fill in empty spaces if the layout used "merged cells"
      #   method: transpose to fill across, the re-transpose to fill down
      t() %>%
      as.data.frame() %>%
      fill(everything(), .direction="down") %>%
      t() %>%
      as.data.frame() %>%
      fill(everything(), .direction="down") %>%
      pivot_longer(cols = everything(), names_to = NULL)
    
    fullLayout <- data.frame(well=wellLayout[[1]], sampleSlot=sampleLayout[[1]], gene=geneLayout[[1]]) %>%
      mutate(well=standardize_well_coords(well))
    
    # import experimental design and merge with layout
    experimentalDesing <- read_excel(input$layoutFile$datapath, sheet = "Experimental Design")
    
    fullDesign <- full_join(fullLayout, experimentalDesing, by=c("sampleSlot"), relationship = "many-to-many")
    
    # import data files and merge with full design
    dataFiles <- read_excel(input$layoutFile$datapath, sheet = "Array Files") 
    
    arrayData <- list()
    for(i in dataFiles$array){
      # in case arrays are not ordered
      dataRow <- which(dataFiles$array == i) 
      filePath <- input$qPCRFiles$datapath[which(input$qPCRFiles$name == dataFiles$filename[dataRow])]
      
      # in case NoCt value is "" (will return NA when imported and not work correctly)
      naValue <- if_else(is.na(dataFiles$NoCtValue[dataRow]), "", dataFiles$NoCtValue[dataRow])
      
      # import data
      arrayData[[i]] <- read_csv(filePath , na=naValue, show_col_types = FALSE) %>%
        rename(well=dataFiles$wellColumnName[dataRow]) %>%
        rename(Ct=dataFiles$CtColumnName[dataRow]) %>%
        mutate(array=i)
    }
    
    arrayData <- bind_rows(arrayData) %>%
      mutate(well=standardize_well_coords(well))
    
    longData <- full_join(fullDesign, arrayData, by=c("well","array")) %>%
      # EXPERIMENTAL FACTORS ARE CURRENTLY HARD CODED. NEED TO MAKE MORE FLEXIBLE
      relocate(well, sampleSlot, sampleName, chemical, dose, replicate, array, gene, Ct) %>%
      mutate(array=as.integer(array)) %>%
      mutate(replicate=as.integer(replicate)) %>%
      mutate(Ct=round(Ct, 1)) %>%
      filter(!is.na(sampleName)) %>% # removes unused sampleSlots
      rename(sample=sampleName)
  }) 
  
  # expand each gene to its own column
  shared$fullData <- reactive({
    shared$longData() %>%
      select(-c(well,sampleSlot)) %>%
      pivot_wider(names_from = gene, values_from = Ct)
  })
  
  # GENE LIST (vector) 
  shared$geneList <- reactive({
    shared$longData() %>%
      pull(gene) %>%
      unique()
  })
  
  # SAMPLE LIST (vector)
  shared$sampleList <- reactive({
    shared$fullData() %>%
      pull(sample)
  })
  
  # FILTERED DATA
  shared$filterData <- reactive({
    req(shared$fullData())
    
    removeGenes <- input$geneFilter
    removeSamples <- input$sampleFilter
    
    shared$fullData() %>%
      filter(!(sample%in%removeSamples)) %>%
      select(-all_of(removeGenes))
  })
  
  
  
  # OUTPUT ----
  output$filterData <- renderReactable(
    reactable(
      shared$filterData(),
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
  
  # DYNAMIC UI ----
  
  # Activate "proceed to QC" buttons
  observeEvent(
    shared$filterData(),{
      updateActionButton(inputId = "toQC", disabled = FALSE)
    })
  
  # Button click navigates to QC page buttons
  observeEvent(
    input$toQC, {
      updateTabsetPanel(inputId = "topTab", selected = "Quality Control")
    })
  
}