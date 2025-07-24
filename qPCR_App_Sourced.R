# TO DO ----

# update "samples summary" results to include all high/Low/NoCt counts, but highlight failed ones with higher percentages
# update "samples summary text"



# Libraries ----
library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(reactable)


# Load Source Code ----
sourcePath <- "Source/"
sourceFiles <- list.files(sourcePath, full.names=TRUE, recursive = TRUE)
walk(sourceFiles, source)


# UI ----
ui <- fluidPage(
  #theme = bs_theme(bootswatch = "flatly"),
  titlePanel("MolTox qPCR Data Analysis App"),
  
  # TOP TABSET ----
  tabsetPanel(
    id = "topTab",
    
    # TAB 1: IMPORT DATA ----
    tabPanel_1_upload_ui, # tabPanel_1_upload_ui.R
    
    # TAB 2: QC (with subtabs) ----
    tabPanel_2_QCsubset_ui, # tabPanel_2_QCsubset_ui.R
    
    # TAB 3: NORMALIZATION ----
    tabPanel_3_NormalizationSubset_ui, # tabPanel_3_NormalizationSubset_ui.R
    
    # TAB 4: DEGS ----
    tabPanel_4_DEGs_ui, # tabPanel_4_DEGs_ui.R
    
    # TAB 5: VISUALIZATION  ----
    tabPanel_5_Visualization_ui # tabPanel_5_Visualization_ui.R
  )
)


# SERVER ----
server <- function(input, output, session){
  
  # Shared Reactives (acts like a list of reactives)
  shared <- reactiveValues()
  
  # TAB1: IMPORT FILES AND CREATE DATA TABLE ----
  tabPanel_1_upload_server(input, output, session, shared)
  
  # TAB 2.1: QC WELLS ----
  tabPanel_2_1_QC_wells_server(input, output, session, shared)
  
  # TAB 2.2: QC SAMPLES/NTC ----
  tabPanel_2_2_QC_samples_server(input, output, session, shared)
  
  # TAB 2.3: QC low High Ct ----
  tabPanel_2_3_QC_lowHighCt_server(input, output, session, shared)
  
  # TAB 2.4: QC Curated Data ----
  tabPanel_2_4_QC_curatedData_server(input, output, session, shared)
  
  # TAB 3.1: Normalization House Keeping Gnes
  tabPanel_3_1_Norm_hkGenes_server
  
  
  # DYANAMIC UI ----
  globalDynamics(input, output, session, shared)

}

# RUN APP ----
shinyApp(ui, server)