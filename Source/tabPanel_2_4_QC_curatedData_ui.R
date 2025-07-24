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
      "Here you can download the raw data table that was made from combining the layout file and the qPCR data files after they were uploaded. The resulting file is a tab-separated text file (.tsv)",
      br(),
      downloadButton("downloadRawData", "Download Raw Data"),
      br(),br(),
      h4("Download Curated Data"),
      "Use the button below to download your curated data table, after all of the QC steps have been applied, as a tab-separated text file (.tsv). ",
      br(),
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