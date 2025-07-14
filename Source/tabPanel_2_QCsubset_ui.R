tabPanel_2_QCsubset_ui <- tabPanel(
  "Quality Control",
  br(),
  
  tabsetPanel(
    id = "qcTab",
    
    # QC TAB 1: Wells
    tabPanel_2_1_QC_wells_ui, # tabPanel_21_QC_wells_ui.R
    
    # QC TAB 2: Samples
    tabPanel_2_2_QC_samples_ui, # tabPanel_22_QC_samples_ui.R
    
    # QC Tab 3: Low/High Ct
    tabPanel_2_3_QC_lowHighCt_ui, # tabPanel_23_QC_lowHighCt_ui.R
    
    # QC Tab 4: Curated Data
    tabPanel_2_4_QC_curatedData_ui # tabPanel_2_4_QC_curatedData_ui.R
    
  )
)