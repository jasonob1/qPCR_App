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



# cleanCt: function to replace No Ct or High Ct values. Can target No Cts or High Cts, and replace with 5 different methods:
# Two possible targets: target = "NoCt" or "HighCt"
# Five possible methods: method = any of the options in the "replaceCtChoices" list above

cleanCt<-function(dataSet, target, method, geneCols, group, ct_thresh, ct_replace=35){
  if(method=="replace with value"){
    cleanData <- dataSet %>%
      mutate(across(
        all_of(geneCols),
        ~{
          if(target=="NoCt"){
            ifelse(is.na(.),ct_replace,.)
          } else if(target == "HighCt"){
            ifelse(.>ct_thresh & !is.na(.),ct_replace,.)
          } else {
            stop("invalid target")
          }
        }
      ))
  }
  
  if(method=="replace with group average"){
    cleanData <- dataSet %>%
      group_by(across(all_of(group))) %>%
      mutate(across(
        all_of(geneCols),
        ~{
          if(target=="NoCt"){
            idx <- is.na(.)
            .[idx] <- round(mean(., na.rm=TRUE),1)
            .
          } else if(target == "HighCt"){
            idx <- .>ct_thresh & !is.na(.)
            .[idx] <- round(mean(., na.rm=TRUE),1)
            .
          } else {
            stop("invalid target")
          }
        }
      )) %>%
      ungroup()
  }
  
  if(method=="replace from random value from group distribution"){
    cleanData <- dataSet %>%
      group_by(across(all_of(group))) %>%
      mutate(across(
        all_of(geneCols),
        ~{
          if (target == "NoCt") {
            idx <- is.na(.)
            .[idx] <- round(rnorm(sum(idx), mean(., na.rm=TRUE), sd(., na.rm=TRUE)),1)
            .
          } else if (target == "HighCt") {
            idx <- . > ct_thresh & !is.na(.)
            .[idx] <- round(rnorm(sum(idx), mean(., na.rm=TRUE), sd(., na.rm=TRUE)),1)
            .
          } else {
            stop("invalid target")
          }
        }
      )) %>%
      ungroup()
  }
  
  if(method=="replace with dataset average"){
    cleanData <- dataSet %>%
      mutate(across(
        all_of(geneCols),
        ~{
          if (target == "NoCt") {
            idx <- is.na(.)
            .[idx] <- round(mean(., na.rm=TRUE),1)
            .
          } else if (target == "HighCt") {
            idx <- . > ct_thresh & !is.na(.)
            .[idx] <- round(mean(., na.rm=TRUE),1)
            .
          } else {
            stop("invalid target")
          }
        }
      )) %>%
      ungroup()
  }
  
  if(method=="replace from random value from dataset distribution"){
    cleanData <- dataSet %>%
      mutate(across(
        all_of(geneCols),
        ~{
          if (target == "NoCt") {
            idx <- is.na(.)
            .[idx] <- round(rnorm(sum(idx), mean(., na.rm=TRUE), sd(., na.rm=TRUE)),1)
            .
          } else if (target == "HighCt") {
            idx <- . > ct_thresh & !is.na(.)
            .[idx] <- round(rnorm(sum(idx), mean(., na.rm=TRUE), sd(., na.rm=TRUE)),1)
            .
          } else {
            stop("invalid target")
          }
        }
      )) 
  }
  
  if(method=="ignore"){
    cleanData<-dataSet
  }
  
  return(cleanData)
}
