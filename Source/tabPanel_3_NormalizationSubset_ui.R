tabPanel_3_NormalizationSubset_ui <- tabPanel(
  "Normalization",
  br(),
  
  tabsetPanel(
    id="normTab",
    
    # Norm Tab 1: HK Genes
    tabPanel_3_1_Norm_hkGenes_ui,
    
    # Norm Tab 2: Normalization
    tabPanel_3_1_Norm_normalization_ui,
    
    # Norm Tab 3: Normalized Data
    tabPanel_3_1_Norm_normData_ui
    
  )
)