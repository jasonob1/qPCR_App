tabPanel_4_DEGs_ui <- tabPanel(
  "DEG analysis",
  br(),
  fluidRow(
    column(
      3,
      h4("Differentially Expressed Gene (DEG) Analysis"),
      br(),
      
      h3("Analysis Options"),
      br(),
      
      selectInput(
        "fcFactor",
        "Select the variable that defines the groups you'd like to compare",
        choices = fcFactorChoices
      ),
      br(),
      
      selectInput(
        "fcControlGroup",
        "Select the Control Group",
        choices = NULL
      ),
      br(),
      
      radioButtons(
        "padjust",
        "Select method to adjust p-values for multiple comparissons",
        choices = list(
          "Raw p-values",
          "Bonferroni correction (conservative)",
          "Benjamini-Hochberg FDR adjustment (recommended)"
        ),
        selected = "Benjamini-Hochberg FDR adjustment (recommended)"
      ),
      br(),
      
      h3("Display Options"),
      br(),
      
      radioButtons(
        "fcScale",
        "Select the scale of the results",
        choices = list(
          "Fold Change",
          "Log2 Fold Change"
        ),
        selected = "Fold Change"
      ),
      br(),
      
      radioButtons(
        "fcFilter",
        "Do you want to show all genes, or filter for significance",
        choices = list(
          "Show all genes",
          "Only show significant differences"
        ),
        selected = "Only show significant differences"
      ),
      br(),
      
      h3("Signficance Filters"),
      numericInput(
        "fcCutoff",
        "Fold change cut-off (value=0 to dissable)",
        min = 0,
        value = 2
      ),
      numericInput(
        "sigCutoff",
        "P-value cut-off (value=1 to dissable)",
        min = 0,
        max = 1,
        value = 0.05
      ),
      br(),
      
      h4("Download Fold Change Table"),
      downloadButton("downloadFoldChangeTable", "Download Fold Change Table"),
      br(),br(),br(),br(),
      
      actionButton("toVis", "Proceed to Visualization Tab", class= "btn-success", disabled = FALSE),
      br(),br()
    ),
    
    column(
      9,
      h4("Fold Change Table"),
      br(),
      reactableOutput("foldChangeTable"),
      br()
    )
  )
)
  
  