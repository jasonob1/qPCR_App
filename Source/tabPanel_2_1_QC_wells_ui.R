tabPanel_2_1_QC_wells_ui <- tabPanel(
  "QC Wells",
  fluidRow(
    column(
      12,
      h3("Quality Control Wells"),
      "Control wells are evaluated for every sample (i.e. every sample has the same control wells)",
      br(),br()
    )
  ),
  # QC Tab 1.1: GDC ----
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
  # QC Tab 1.2: RTC ----
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
  # QC Tab 1.3: PPC ----
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
  ),
  # "to QC Samples" Button
  fluidRow(
    column(
      12,
      br(),
      actionButton("toQCsamp", "Proceed to QC Samples", class= "btn-success", disabled = FALSE),
      br(),br()
    )
  )
)