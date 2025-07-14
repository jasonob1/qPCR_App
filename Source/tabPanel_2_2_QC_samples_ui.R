tabPanel_2_2_QC_samples_ui <- tabPanel(
  "QC Samples",
  fluidRow(
    column(
      12,
      h3("Quality Control Samples"),
      "Control Samples are evaluated across every gene and WILL NOT not be included in the final analysis",
      br(),br()
    )
  ),
  # QC Tab 2.1: NTC ----
  fluidRow(
    column(
      3,
      h4("No Template Controls (NTC)"),
      "No template controls are full samples that check for amplification across all genes. They do not include RNA in the reverse transcription reaction, so they should not have amplification for any gene",
      br(),br(),
      selectInput("ntc", "Select NTC Samples", choices = NULL, multiple = TRUE),
      sliderInput("ntcThresh", "Select NTC threshold", min=1, max=40, value=35),
      br()
    ),
    column(
      3,
      h4("NTC Summary"),
      br(),
      reactableOutput("ntcSummary")
    ),
    column(
      6,
      h4("NTC Results:"),
      br(),
      reactableOutput("ntcTest"),
      br()
    )
  ),
  fluidRow(
    column(
      12,
      br(),
      actionButton("toLowHigh", "Proceed to Low/High Ct Check", class= "btn-success", disabled = FALSE),
      br(),br()
    )
  )
)