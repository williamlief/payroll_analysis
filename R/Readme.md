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
downloaded files. All raw files are located in the companion OSF repository at XXXX. Some of the first cleaning work was done in stata, that code will need to be migrated to R. Code quality is uneven, and would benefit from refactoring to be made consistent. 

1. core_variables.txt list the names of the core set of variables that were processed into the master dataset. 
1. `MasterCompilation.R` pulls together the csv files for each state and writes `data-raw/clean_payroll_records.RDS`.
1. All other files are self explanatory and process the files for the respective state.
