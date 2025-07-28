tabPanel_3_1_Norm_hkGenes_ui <- tabPanel(
  "Housekeeping Genes",
  fluidRow(
    column(
      3,
      h3("Housekeeping Genes"),
      br(),
      selectInput("hkg", "Select Housekeeping Genes", choices = NULL, multiple = TRUE),
      br(),br(),
      selectInput(
        "hkFactor",
        "Select the variable that defines the groups you'd like to compare using ANOVA",
        choices = hkFactorChoices,
        multiple = FALSE # for now only one-way ANOVA
      ),
      br(),br(),
      
      actionButton("toNormNorm", "Proceed to Data Normalization", class= "btn-success", disabled = FALSE),
      br(),br()
    ),
    column(
      9,
      h4("Housekeeping Gene ANOVA test"),
      reactableOutput("hkANOVA"),
      br(),br(),
      
      h4("Group-specific p-values"),
      reactableOutput("hkMultiComp"),
      br(),br(),
      
      h4("Housekeeping Gene Plots"),
      plotOutput("hkPlot")
      
      
    )
  )
)