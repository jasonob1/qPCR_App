tabPanel_3_3_Norm_normData_ui <- tabPanel(
  "Normalized Data",
  fluidRow(
    column(
      12,
      h3("Normalized Data"),
      "Here, you can download a table of the normalized data",
      br(),br()
    )
  ),
  fluidRow(
    column(
      2,
      h4("Download Normalized Data"),
      "Click below to download the normalized data",
      br(),
      downloadButton("downloadNormData", "Download Normalized Data"),
      br(),br(),
      
      actionButton("toDEGs", "Proceed to DEG analysis", class= "btn-success", disabled = FALSE),
      br(),br()
    ),
    column(
      10,
      h3("Preview of Normalized Data"),
      br(),
      reactableOutput("normalizedData")
    )
  )
)