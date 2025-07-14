globalDynamics <- function(input, output, session, shared){
  
  # DYNAMIC UI ----
  
  # update selectInputs with geneList names
  observeEvent(shared$geneList(),{
    geneChoice <- c(shared$geneList())
    updateSelectInput(inputId = "gdc", choices = geneChoice)
    updateSelectInput(inputId = "rtc", choices = geneChoice) 
    updateSelectInput(inputId = "ppc", choices = geneChoice)
    updateSelectInput(inputId = "hkg", choices = geneChoice)
    updateSelectInput(inputId = "geneFilter", choices = geneChoice)
  })
  
  # update selectInputs with sampleList
  observeEvent(shared$sampleList(),{
    sampleChoice <- c(shared$sampleList())
    updateSelectInput(inputId = "sampleFilter", choices = sampleChoice)
    updateSelectInput(inputId = "ntc", choices = sampleChoice)
  })
  
}
  
  