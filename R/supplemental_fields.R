# Rough file for mapping out extra field availability

library(tidyverse)

raw <- readRDS("data-raw/clean_payroll_records.RDS")
core_vars <- names(raw)
other_dont_care <- c("salary_base", "salary_overtime", "salary_other", 
                     "salary_benefits", "salary_total_benefits","school_district", 
                     "NCES_localidid", "source", "county", "city", "district", "year2",
                     "salary_total", "location", "NCES_name", "localid", "NCES_type")
remove_vars <- c(core_vars, other_dont_care)

# cleaned csvs live in a separate project where i've compiled all the raw
# TODO: Get everything together on a hosted repo
path <- "C:\\Users\\liefe\\Documents\\DistrictVariation\\PayrollData"

files <- list.files(path, pattern = "*.csv", full.names = TRUE)
names(files) <- str_match(files, "(.*PayrollData/)(\\D{2})(_Payroll.csv)")[,3]

vars <- map(files, read_csv, n_max = 0) %>% 
  map(names)

map(vars, function(x) x[!x %in% remove_vars])

# Manually build a table from this output