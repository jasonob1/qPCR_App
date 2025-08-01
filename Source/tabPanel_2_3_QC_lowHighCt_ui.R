tabPanel_2_3_QC_lowHighCt_ui <- tabPanel(
  "Low or High Ct Check",
  fluidRow(
    column(
      12,
      h3("Low and High Ct Check"),
      "Check for Genes or Samples with a high proportion of low or high Cts",
      br(),br()  
    )
  ),
  fluidRow(
    column(
      2,
      h4("Ct Thresholds"),
      br(),
      sliderInput("lowCt", "select low Ct threshold", min=1, max=40, value=15),
      br(),
      sliderInput("highCt", "select high Ct threshold", min=1, max=40, value=35),
      br(),
      sliderInput("lowHighPerc", "select percent of samples threshold", min=0, max=100, value=50),
      "How many samples must be outside the cutoffs for a gene to fail the test?",
      br(),br(),br(),
      
      h4("Replace No Ct and High Ct Options"),
      br(),
      
      radioButtons(
        "replaceNoCt",
        "What would you like to do with any remaining No Ct (i.e. missing) values?",
        choices = replaceCtChoices,
        selected = "ignore"
      ),
      br(),br(),
      
      radioButtons(
        "replaceHighCt",
        "What would you like to do with any remaining high Ct (above threshold) values?",
        choices = replaceCtChoices,
        selected = "ignore"
      ),
      br(),br(),
      
      numericInput(
        "replaceCtvalue",
        "Replacement value (only for 'replace with value' option)",
        value=35
      ),
      br(),br(),
      
      actionButton("toCuratedData", "View/Download Curated Data", class= "btn-success", disabled = FALSE),
      br(),br()
    ),
    
    column(
      4,
      h4("Low or High Ct Summary"),
      br(),
      h4("Genes"),
      textOutput("lowHighCtGeneSumText"),
      reactableOutput("lowHighCtGeneSummary"),
      br(),
      h4("Samples"),
      textOutput("lowHighCtSampleSumText"),
      reactableOutput("lowHighCtSampleSummary"),
      br()
    ),
    
    column(
      6,
      h4("Detailed Low or High Ct Results:"),
      br(),
      reactableOutput("lowHighCtTest"),
      br()
    )
  ),
  fluidRow(
    
  )
)