### Data cleaning

This is the heart of the project. I've been working on this for years, and learned a ton about coding over that time. Code quality has improved dramatically from some of the early files. In fact, the earliest work was done in Stata, that code will need to be migrated to R. Code quality is uneven, and would benefit from refactoring to be made consistent. Over the years I had issues with storage capacity and computational constraints and the project moved around a bit. I have now stored all the raw data (except NY) on the OSF at XXX where it can be downloaded. File directories will need to be updated to get things running. 

1. `libs_and_paths.R` creates a couple of path variables with the location of the raw data and the place to save the output file. Loads some libraries as well.
1. core_variables.txt list the names of the core set of variables that were processed into the master dataset. 
1. `MasterCompilation.R` pulls together the csv files for each state and writes `data-raw/clean_payroll_records.RDS`.
1. `helper_functions/` contains a couple of helper functions that I think are only used in the CA script. It was an early attempt at refactoring and updating code that didn't get followed through on. 
1. All other files are self explanatory and process the files for the respective state.