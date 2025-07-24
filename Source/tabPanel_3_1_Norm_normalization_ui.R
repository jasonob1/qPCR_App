tabPanel_3_1_Norm_normalization_ui <- tabPanel(
  "Normalization",
  fluidRow(
    column(
      3,
      h3("Normalization Method"),
      br(),
      radioButtons(
        "normMethod",
        "Select the normalization method",
        choices = list(
          "house keeping genes",
          "Trimmed mean of M values (TMM)",
          "none"
        ),
        selected = "none"
      )
    )
  )
)