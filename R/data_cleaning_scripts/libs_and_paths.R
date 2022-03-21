# library(pdftools)
library(tidyverse)
# library(readxl)
# library(haven)
# library(ggridges)
# library(lme4)
# library(lfe)
# library(knitr)


# helper functions - i think these are only used in CA, an experiment
source("PayrollData/code/helperFunctions/clean_names.R")
source("PayrollData/code/helperFunctions/missing_summary.R")


# Historically data was stored on an external ssd disk due to memory constraints
# Update these to point to the directory where you downloaded the raw files to  
stubs <- list()
stubs$dh <- "" # Set to location you download osf files to. I recmomend this repo / data-raw/payroll
stubs$cl <- "C:/Users/liefe/Documents/DistrictVariation/" # where to save clean csvs, I recommend this repo/data/

paths <- list()
# Location of a file with NCES CCD records, if downloading from osf nothing additional needed
paths$ccd_district <- paste0(stubs$dh,"")
paths$payroll <- paste0(stubs$cl,"PayrollData/") # clean payroll results save location