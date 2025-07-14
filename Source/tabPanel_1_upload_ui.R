tabPanel_1_upload_ui <- tabPanel(
  "Upload Data",
  column(
    2,
    h3("Upload Your Data"),
    br(),
    fluidRow(
      fileInput("layoutFile", "Upload Layout File", buttonLabel = "Upload"),
      fileInput("qPCRFiles", "Upload qPCR Data Files", buttonLabel = "Upload", multiple=TRUE)
    ),
    fluidRow(
      h3("Filter Your Data"),
      br(),
      selectInput("sampleFilter", label="Select Samples to Remove", choices = NULL, multiple = TRUE),
      selectInput("geneFilter", label="Select Genes to Remove", choices = NULL, multiple = TRUE),
      br(),
      br()
    ),
    fluidRow(
      br(),
      actionButton("toQC", "Proceed to QC Check", class= "btn-success", disabled = TRUE)
    )
  ),
  column(
    10,
    h3("Data Preview:"),
    br(),
    reactableOutput("filterData")
  )
)