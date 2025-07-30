tabPanel_3_2_Norm_normalization_ui <- tabPanel(
  "Normalization",
  fluidRow(
    column(
      3,
      h4("Normalization Method"),
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
      ),
      br(),br(),
      
      actionButton("toNormData", "Proceed to Normalized Data Download", class= "btn-success", disabled = FALSE),
      br(),br()
    ),
    column(
      9,
      h4("pre-normalization distribution of Cts"),
      plotOutput("preNormDist", height = 400),
      br(),
      h4("Distribution of Normalized Cts"),
      plotOutput("normDist", height = 400),
      br()
    )
  )
)