tabPanel_2_4_QC_curatedData_ui <- tabPanel(
  "Curated Data",
  fluidRow(
    column(
      12,
      h3("Curated Data"),
      "This is the final version of the data to be analyzed. All indicated genes and samples have been removed (including QC genes and samples). Low, High, or missing Cts have been adjusted as determined on the 'Low or High Ct Check' tab. Here you can download tables of the original Raw data or the final curated data that can be used in other tools.",
      br(),br()
    )
  ),
  fluidRow(
    column(
      2,
      h4("Download Raw Data"),
      downloadButton("downloadRawData", "Download Raw Data"),
      br(),br(),
      h4("Download Curated Data"),
      downloadButton("downloadCuratedData", "Download Curated Data")
    ),
    column(
      10,
      h3("Preview of Curated Data"),
      br(),
      reactableOutput("curatedData")
    )
  )
)