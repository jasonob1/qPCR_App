tabPanel_3_Normalization_ui <- tabPanel(
  "Normalization",
  br(),
  tabsetPanel(
    id="normTab",
    # Norm Tab 1: HK Genes ----
    tabPanel(
      "Housekeeping Genes",
      fluidRow(
        column(
          3,
          h3("Housekeeping Genes"),
          br(),
          selectInput("hkg", "Select Housekeeping Genes", choices = NULL, multiple = TRUE),
        )
      )
    )
  )
)