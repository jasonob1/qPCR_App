# FUNCTIONS ----

# Standardize to Remove leading zeros from single-digit well numbers
standardize_well_coords <- function(wells) {
  gsub("([A-H])0([1-9])", "\\1\\2", wells)
}



# Replace Ct option list and function ----
replaceCtChoices <- c(
  "replace with value",
  "replace with group average",
  "replace from random value from group distribution",
  "replace with dataset average",
  "replace from random value from dataset distribution",
  "ignore"
)

cleanNoCt<-function(dataSet, method, geneCols, group, ct=35){
  if(method=="replace with value"){
    cleanData <- dataSet %>%
      mutate(across(
          all_of(geneCols),
          ~ifelse(is.na(.),ct,.)   # TO DO?: add a "target" argument to cleanNoCt, with options "NoCt" and "HighCt", then use "target" as a switch
        ))
  }
  
  if(method=="replace with group average"){
    cleanData <- dataSet %>%
      group_by(all_of(group)) %>%
      mutate(across(
          all_of(geneCols),
          ~ifelse(is.na(.),mean(., na.rm=TRUE),.)
        )) %>%
      ungroup()
  }
  
  if(method=="replace from random value from group distribution"){
    cleanData <- dataSet %>%
      group_by(all_of(group)) %>%
      mutate(across(
        all_of(geneCols),
        ~sapply(.,
          FUN=function(x){
            ifelse(is.na(x),rnorm(1, mean(., na.rm=TRUE),sd(., na.rm=TRUE)),x)
          }
        )
      )) %>%
      ungroup()
  }
  
  if(method=="replace with dataset average"){
    cleanData <- dataSet %>%
      mutate(across(
        all_of(geneCols),
        ~ifelse(is.na(.),mean(., na.rm=TRUE),.)
      )) %>%
      ungroup()
  }
  
  if(method=="replace from random value from dataset distribution"){
    cleanData <- dataSet %>%
      mutate(across(
        all_of(geneCols),
        ~sapply(.,
          FUN=function(x){
            ifelse(is.na(x),rnorm(1, mean(., na.rm=TRUE),sd(., na.rm=TRUE)),x)
          }
        )
      )) %>%
      ungroup()
  }
  
  if(method=="ignore"){
    cleanData<-dataSet
  }
  
  return(cleanData)
}
