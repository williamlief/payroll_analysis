# File guide 

## Linkage Files
Files that test different linkage methods. 
1. `deterministic_iter.R` a deterministic match, this is better implemented in ensemble_link.R
1. `fastLink_iter.R` an implementation of fastLink, better done in ensemble_link.R
1. `ensemble_link.R` Deterministic + fastlink, this is the best and cleanest code. Needs work 
to generalize to new data sets. 

## Compile Data 

1. `compile_linked_data.R` Adds the ensemble_id onto the raw payroll records.

## Analysis Scripts 

1. `data_availability.R` - Quick table of years by state in compiled data set
1. `Linkage_analysis.R` - creates estimates of linkage quality using Oklahoma data
1. `survival_analysis.R` - Turnover plots/tables and median survival time analysis
1. `supplemental_fields.R` - analysis of raw csv files with additional fields present

## data_cleaning_scripts/ 

Scripts for cleaning the raw files. In some cases there was minimal hand processing of 
downloaded files. All raw files are located in the companion OSF repository at XXXX. 
