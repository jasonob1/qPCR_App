# Formatting ----
failCol <- "#dc3815"
failFont <- "bold"
passCol <- "black"
passFont <- "normal"


# Style Functions ----
gdc_style_fun <- function(.data, colname, thresh){
  function(value){
    
    if(is.na(value)){
      color <- passCol
      fontWeight <- passFont
    } else if (is.numeric(value)) {
      if(value<thresh){
        color <- failCol
        fontWeight <- failFont
      }else{
        color <- passCol
        fontWeight <- passFont
      }
    } else if (is.character(value)) {
      if(value=="FAIL"){
        color <- failCol
        fontWeight <- failFont
      }else{
        color <- passCol
        fontWeight <- passFont
      }
    } else {
      color <- passCol
      fontWeight <- passFont
    }
    return(
      list(color=color, fontWeight=fontWeight)
    )
  }
}

rtc_ppc_style_fun <- function(.data, colname, thresh){
  function(value){
    
    if(is.na(value)){
      color <- failCol
      fontWeight <- failFont
    } else if (is.numeric(value)) {
      if(value<=thresh){
        color <- passCol
        fontWeight <- passFont
      }else{
        color <- failCol
        fontWeight <- failFont
      }
    } else if (is.character(value)) {
      if(value=="FAIL"){
        color <- failCol
        fontWeight <- failFont
      }else{
        color <- passCol
        fontWeight <- passFont
      }
    } else {
      color <- passCol
      fontWeight <- passFont
    }
    return(
      list(color=color, fontWeight=fontWeight)
    )
  }
}

ntc_style_fun <- function(.data, colname, thresh){
  function(value){
    
    if(is.na(value)){
      color <- passCol
      fontWeight <- passFont
    } else if (is.numeric(value)) {
      if(value<thresh){
        color <- passCol
        fontWeight <- passFont
      }else{
        color <- failCol
        fontWeight <- failFont
      }
    } else if (is.character(value)) {
      if(value=="FAIL"){
        color <- failCol
        fontWeight <- failFont
      }else{
        color <- passCol
        fontWeight <- passFont
      }
    } else {
      color <- passCol
      fontWeight <- passFont
    }
    return(
      list(color=color, fontWeight=fontWeight)
    )
  }
}
