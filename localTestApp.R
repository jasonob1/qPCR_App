# Libraries ----
library(tidyverse)
library(readxl)
library(reactable)

# Functions ----
standardize_well_coords <- function(wells) {
  gsub("([A-H])0([1-9])", "\\1\\2", wells)
}

# directories and file names ----
fileDir <- "Data\\Dania LMH PE and POCIS March 2025\\"
layoutFile <- "96 well Array and Sample Layout -Danie PE Test.xlsx"

#fileDir <- "Data\\LMH-Wetlands 384 array 2024\\"
#layoutFile<-"Array and Sample Layout -Laura AOSR Wetland Dose Response Array.xlsx"


# APP CODE ----
  # import layouts
  wellLayout <- read_excel(paste0(fileDir,layoutFile), sheet = "Well Layout") %>%
    column_to_rownames("well") %>%
    pivot_longer(cols = everything(), names_to = NULL)
  
  geneLayout <- read_excel(paste0(fileDir,layoutFile), sheet = "Gene Layout") %>%
    column_to_rownames("well") %>%
    pivot_longer(cols = everything(), names_to = NULL)
  
  sampleLayout <- read_excel(paste0(fileDir,layoutFile), sheet = "Sample Layout") %>%
    column_to_rownames("well") %>%
    # need to fill in empty spaces if the layout used "merged cells"
    #   method: transpose to fill across, the re-transpose to fill down
    t() %>%
    as.data.frame() %>%
    fill(everything(), .direction="down") %>%
    t() %>%
    as.data.frame() %>%
    fill(everything(), .direction="down") %>%
    pivot_longer(cols = everything(), names_to = NULL)
  
  fullLayout <- data.frame(well=wellLayout[[1]], sampleSlot=sampleLayout[[1]], gene=geneLayout[[1]]) %>%
    mutate(well=standardize_well_coords(well))
  
  
  # import experimental design and merge with layout
  experimentalDesing <- read_excel(paste0(fileDir,layoutFile), sheet = "Experimental Design")
  
  fullDesign <- full_join(fullLayout, experimentalDesing, by=c("sampleSlot"), relationship = "many-to-many")
  
  
  # import data files and merge with full design
  dataFiles <- read_excel(paste0(fileDir,layoutFile), sheet = "Array Files")
  
  arrayData <- list()
  for(i in dataFiles$array){
    dataRow <- which(dataFiles$array == i)
    naValue <- if_else(is.na(dataFiles$NoCtValue[i]), "", dataFiles$NoCtValue[i])
    arrayData[[i]] <- read_csv(paste0(fileDir,dataFiles$filename[i]), na=naValue, show_col_types = FALSE) %>%
      rename(well=dataFiles$wellColumnName[dataRow]) %>%
      rename(Ct=dataFiles$CtColumnName[dataRow]) %>%
      mutate(array=i)
  }
  
  arrayData <- bind_rows(arrayData) %>%
    mutate(well=standardize_well_coords(well))
  
  
  longData <- full_join(fullDesign, arrayData, by=c("well","array")) %>%
    # EXPERIMENTAL FACTORS ARE CURRENTLY HARD CODED. NEED TO MAKE MORE FLEXIBLE
    relocate(well, sampleSlot, sampleName, chemical, dose, replicate, array, gene, Ct) %>%
    mutate(array=as.integer(array)) %>%
    mutate(replicate=as.integer(replicate)) %>%
    mutate(Ct=round(Ct, 1)) %>%
    filter(!is.na(sampleName)) %>% # removes unused sampleSlots
    rename(sample=sampleName)

# expand each gene to its own column
fullData <- longData %>%
    select(-c(well,sampleSlot)) %>%
    pivot_wider(names_from = gene, values_from = Ct)


# GENE LIST (vector) ----
geneList <- longData %>%
    pull(gene) %>%
    unique()


# QC SECTION ----

# GDC Test

gdc <- c("FGF19", "GDC")
gdcThresh <- 35

gdcTest<-fullData %>%
  select(sample, all_of(gdc)) %>%
  mutate(across(all_of(gdc), 
    list(
      TEST = ~.x>gdcThresh | is.na(.x)
    )
  )) %>%
  select(order(colnames(.))) %>%
  relocate(sample)



# RTC Test

rtc <- c("FGF19", "RTC")
rtcThresh <- 30

rtcTest<-fullData %>%
  select(sample, all_of(rtc)) %>%
  mutate(across(all_of(rtc), 
                list(
                  TEST = ~.x<rtcThresh & !is.na(.x)
                )
  )) %>%
  select(order(colnames(.))) %>%
  relocate(sample)


#GDC Summary

# Define pass columns
test_columns <- names(gdcTest)[grepl("_TEST$", names(gdcTest))]

# Initialize result data frame
outcomes <- c(TRUE, FALSE)
result <- data.frame(outcome = outcomes)

# Loop through each pass column and add counts
for(col in test_columns) {
  counts <- table(gdcTest[[col]])
  
  # Make sure both TRUE and FALSE are represented
  result[[col]] <- 0  # Initialize with zeros
  
  # Fill in actual counts where available
  for(outcome in names(counts)) {
    idx <- which(result$outcome == as.logical(outcome))
    result[idx, col] <- counts[outcome]
  }
}


# HighLow Ct Test

GOIs <- names(fullData)[names(fullData)%in%geneList]
lowCt <- 15
highCt <- 35


fullData %>%
  select(all_of(GOIs)) %>%
  mutate(across(everything(), ~.x>lowCt|is.na(.x))) %>%
  colSums()/nrow(fullData) *100






