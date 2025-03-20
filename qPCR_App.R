# Libraries ----
library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(reactable)

# CONSTANTS AND FUNCTIONS ----
# Remove leading zeros from single-digit well numbers
failCol <- "#dc3815"
failFont <- "bold"
passCol <- "black"
passFont <- "normal"

standardize_well_coords <- function(wells) {
  gsub("([A-H])0([1-9])", "\\1\\2", wells)
}

gdc_style_fun <- function(.data, colname, thresh){
  function(value){
    
    if(is.na(value)){
      color <- passCol
      fontWeight <- passFont
    } else if (is.numeric(value)) {
      if(value<thresh){
        color <- failCol
        fontWeight <- failFont
      }else{
        color <- passCol
        fontWeight <- passFont
      }
    } else if (is.character(value)) {
      if(value=="FAIL"){
        color <- failCol
        fontWeight <- failFont
      }else{
        color <- passCol
        fontWeight <- passFont
      }
    } else {
      color <- passCol
      fontWeight <- passFont
    }
    return(
      list(color=color, fontWeight=fontWeight)
    )
  }
}

rtc_ppc_style_fun <- function(.data, colname, thresh){
  function(value){
    
    if(is.na(value)){
      color <- failCol
      fontWeight <- failFont
    } else if (is.numeric(value)) {
      if(value<=thresh){
        color <- passCol
        fontWeight <- failFont
      }else{
        color <- failCol
        fontWeight <- failFont
      }
    } else if (is.character(value)) {
      if(value=="FAIL"){
        color <- failCol
        fontWeight <- failFont
      }else{
        color <- passCol
        fontWeight <- passFont
      }
    } else {
      color <- passCol
      fontWeight <- passFont
    }
    return(
      list(color=color, fontWeight=fontWeight)
    )
  }
}


# UI ----
ui <- fluidPage(
  #theme = bs_theme(bootswatch = "flatly"),
  titlePanel("MolTox qPCR Data Analysis App"),
  
  # TOP TABSET ----
  tabsetPanel(
    id = "topTab",
    
    # TAB 1: IMPORT DATA ----
    tabPanel(
      "Upload Data",
      column(
        2,
        h3("Upload Your Data"),
        br(),
        fluidRow(
          fileInput("layoutFile", "Upload Layout File", buttonLabel = "Upload"),
          fileInput("qPCRFiles", "Upload qPCR Data Files", buttonLabel = "Upload", multiple=TRUE),
        ),
        fluidRow(
          br(),
          actionButton("toQC", "Proceed to QC Check", class= "btn-success", disabled = TRUE)
        )
      ),
      column(
        10,
        h3("Data Preview:"),
        br(),
        reactableOutput("fullData")
      )
    ),
    
    # TAB 2: QC  ----
    tabPanel(
      "Quality Control",
      br(),
      tabsetPanel(
        id = "qcTab",
        # QC Tab 1: Wells ----
        tabPanel(
          "QC Wells",
          fluidRow(
            column(
              12,
              h3("Quality Control Wells"),
              "Control wells are evaluated for every sample (i.e. every sample has the same control wells)",
              br(),br()
            )
          ),
          # QC TAB 1.1: GDC ----
          fluidRow(
            column(
              3,
              h4("Genomic DNA Control (GDC)"),
              "GDCs check for genomic DNA contamination in your samples. They should have minimal (preferably zero) amplification",
              br(),br(),
              selectInput("gdc", label="Select GDC Well(s)", choices = NULL, multiple = TRUE),
              sliderInput("gdcThresh", "Select GDC threshold", min=1, max=40, value=35),
              br(),
              br()
            ),
            column(
              3,
              h4("GDC Summary"),
              br(),
              reactableOutput("gdcSummary")
            ),
            column(
              6,
              h4("GDC Results:"),
              br(),
              reactableOutput("gdcTest"),
              br()
            )
          ),
          # QC TAB 1.2: RTC ----
          fluidRow(
            column(3,
              h4("Reverse Transcription Control (RTC)"),
              "RTCs check for if the RT reaction was successful for your sample. They should have good amplification",
              br(),br(),
              selectInput("rtc", "Select RTC Well(s)", choices = NULL, multiple = TRUE),
              sliderInput("rtcThresh", "Select RTC threshold", min=1, max=40, value=30),
              br()
            ),
            column(
              3,
              h4("RTC Summary"),
              br(),
              reactableOutput("rtcSummary")
            ),
            column(
              6,
              h4("RTC Results:"),
              br(),
              reactableOutput("rtcTest")
            )
          ),
          # QC TAB 1.3: PPC ----
          fluidRow(
            column(3,
              h4("PCR Positive Control (PPC)"),
              "PPCs check if your PCR mastermix works correctly. They should have good amplification",
              br(),br(),
              selectInput("ppc", "Select PPC Well(s)", choices = NULL, multiple = TRUE),
              sliderInput("ppcThresh", "Select PPC threshold", min=1, max=40, value=30),
              br()
            ),
            column(
              3,
              h4("PPC Summary"),
              br(),
              reactableOutput("ppcSummary")
            ),
            column(
              6,
              h4("PPC Results:"),
              br(),
              reactableOutput("ppcTest")
            )
          )
        ),
        # QC Tab 2: Samples ----
        tabPanel(
          "QC Samples",
          fluidRow(
            column(
              3,
              h3("Select Control Samples"),
              br(),
              selectInput("rtc", "Reverse Transcription Control", choices = NULL, multiple = TRUE),
              br(),
              selectInput("ppc", "PCR Postive Control", choices = NULL, multiple = TRUE),
              br(),
              selectInput("ntc", "No Template Control", choices = NULL, multiple = TRUE),
              br()
            ),
            column(9)
          )
        ),
        # QC Tab 3: High/Low ----
        tabPanel(
          "Low or High Ct Genes",
          fluidRow(
            column(
              3,
              h3("Ct Thresholds"),
              br(),
              sliderInput("lowCt", "select low Ct threshold", min=1, max=40, value=15),
              br(),
              sliderInput("highCt", "select high Ct threshold", min=1, max=40, value=35),
              br()
            ),
            column(9)
          )
        )
      )
    ),
    
    # TAB 4: NORMALIZATION ----
    tabPanel(
      "Normalization",
      br(),
      tabsetPanel(
        id="normTab",
        # Norm Tab 1: HK Genes ----
        tabPanel(
          "Housekeeping Genes",
          fluidRow(
            column(
              3,
              h3("Housekeeping Genes"),
              br(),
              selectInput("hkg", "Select Housekeeping Genes", choices = NULL, multiple = TRUE),
            )
          )
        )
      )
    ),
    
    # TAB 4: DEGS ----
    tabPanel("DEGs"),
    
    # TAB 5: VISUALIZATION  ----
    tabPanel("Visualization")
  )
)






# SERVER ----
server <- function(input, output, session){
  # TAB1: IMPORT FILES AND CREATE DATA TABLE ----
    # COMPUTE ----
    longData <- reactive({
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
    fullData <- reactive({
      longData() %>%
        select(-c(well,sampleSlot)) %>%
        pivot_wider(names_from = gene, values_from = Ct)
    })
  
    # OUTPUT ----
    output$fullData <- renderReactable(
      reactable(
        fullData(),
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
    # GENE LIST (vector) ----
    geneList <- reactive({
      longData() %>%
        pull(gene) %>%
        unique()
    })
  
  
  # TAB 2.1: QC WELLS ----
    # COMPUTE ----
    
    # GDC Test
    gdcTest <- reactive({
      req(input$gdc, input$gdcThresh)
      
      gdc <- input$gdc
      gdcThresh <- input$gdcThresh
      
      fullData() %>%
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
    gdcSummary <-reactive({
      req(gdcTest())
      
      # Define pass columns
      test_columns <- names(gdcTest())[grepl("_TEST$", names(gdcTest()))]
      
      # Initialize result data frame
      outcomes <- c(TRUE, FALSE)
      result <- data.frame(outcome = outcomes)
      
      # Loop through each pass column and add counts
      for(col in test_columns) {
        counts <- table(gdcTest()[[col]])
        
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
    rtcTest <- reactive({
      req(input$rtc, input$rtcThresh)
      
      rtc <- input$rtc
      rtcThresh <- input$rtcThresh
      
      fullData() %>%
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
    rtcSummary <-reactive({
      req(rtcTest())
      
      # Define pass columns
      test_columns <- names(rtcTest())[grepl("_TEST$", names(rtcTest()))]
      
      # Initialize result data frame
      outcomes <- c(TRUE, FALSE)
      result <- data.frame(outcome = outcomes)
      
      # Loop through each pass column and add counts
      for(col in test_columns) {
        counts <- table(rtcTest()[[col]])
        
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
    ppcTest <- reactive({
      req(input$ppc, input$ppcThresh)
      
      ppc <- input$ppc
      ppcThresh <- input$ppcThresh
      
      fullData() %>%
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
    ppcSummary <-reactive({
      req(ppcTest())
      
      # Define pass columns
      test_columns <- names(ppcTest())[grepl("_TEST$", names(ppcTest()))]
      
      # Initialize result data frame
      outcomes <- c(TRUE, FALSE)
      result <- data.frame(outcome = outcomes)
      
      # Loop through each pass column and add counts
      for(col in test_columns) {
        counts <- table(ppcTest()[[col]])
        
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
      req(gdcTest())
      
      ctCols <- input$gdc
      testCols <- ctCols %>%
        str_c("_TEST")
      
      formatCols <- c(input$gdc, testCols)
      
      toRender <- gdcTest() %>%
        mutate(across(all_of(testCols), ~ ifelse(.x, "PASS", "FAIL"))) %>%
        rename_with(~paste0(., "_Ct"), all_of(ctCols))
      
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
            list(background = "#f8e6e6")
          }
        },
        fullWidth = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100)
      )
    })
    
    #GDC Summary
    output$gdcSummary <- renderReactable({
      req(gdcSummary())
      toRender <- gdcSummary() %>%
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
              background = "#f8e6e6",
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
      req(rtcTest())
      
      ctCols <- input$rtc
      testCols <- ctCols %>%
        str_c("_TEST")
      
      formatCols <- c(input$rtc, testCols)
      
      toRender <- rtcTest() %>%
        mutate(across(all_of(testCols), ~ ifelse(.x, "PASS", "FAIL"))) %>%
        rename_with(~paste0(., "_Ct"), all_of(ctCols))
      
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
            list(background = "#f8e6e6")
          }
        },
        fullWidth = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100)
      )
    })
    
    #RTC Summary
    output$rtcSummary <- renderReactable({
      req(rtcSummary())
      toRender <- rtcSummary() %>%
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
              background = "#f8e6e6",
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
      req(ppcTest())
      
      ctCols <- input$ppc
      testCols <- ctCols %>%
        str_c("_TEST")
      
      formatCols <- c(input$ppc, testCols)
      
      toRender <- ppcTest() %>%
        mutate(across(all_of(testCols), ~ ifelse(.x, "PASS", "FAIL"))) %>%
        rename_with(~paste0(., "_Ct"), all_of(ctCols))
      
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
            list(background = "#f8e6e6")
          }
        },
        fullWidth = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100)
      )
    })
    
    #PPC Summary
    output$ppcSummary <- renderReactable({
      req(ppcSummary())
      toRender <- ppcSummary() %>%
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
              background = "#f8e6e6",
              color = failCol,
              fontWeight = failFont
            )
          }
        },
        fullWidth = TRUE
      )
    })
    
  
  # DYNAMIC UI ----
  
  # Activate "proceed" buttons
  observeEvent(
    fullData(),{
      updateActionButton(inputId = "toQC", disabled = FALSE)
    })
  
  # Tab navigation buttons
  observeEvent(
    input$toQC, {
      updateTabsetPanel(inputId = "topTab", selected = "Quality Control")
  })
  
  # update QC selectInputs with geneList names
  observeEvent(geneList(),{
    sampleChoice <- c("NOT USED", geneList())
    updateSelectInput(inputId = "gdc", choices = sampleChoice)
    updateSelectInput(inputId = "rtc", choices = sampleChoice) 
    updateSelectInput(inputId = "ppc", choices = sampleChoice) 
  })
  
  # update QC HKG selectInputs with geneList
  observeEvent(geneList(),{
    geneChoice <- c("NOT USED", geneList())
    updateSelectInput(inputId = "hkg", choices = geneChoice) 
  })
  
  
  
}

# RUN APP ----
shinyApp(ui, server)