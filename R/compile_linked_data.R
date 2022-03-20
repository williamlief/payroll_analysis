library(tidyverse)
library(collapse)

original <- readRDS("data-raw/clean_payroll_records.RDS")

source_id_states = c("OK", "ND")

# use the ensemble method
files <- list.files(path = "data/ensemble/", pattern = "*.RDS", full.names = TRUE)

raw <- map_dfr(files, readRDS)

# Set the primary id that will be used in all analysis
# use source_id in source_id states
# IL and WI do have source ids but they aren't available for all years

dat <- raw %>% 
  select(-starts_with("ensemble_id_")) %>% 
  # Add back in ND, it didn't get linked because it only provided IDs and no names
  bind_rows(original %>% filter(state == "ND")) %>%
  mutate(id = ensemble_id, 
         id = if_else(state %in% source_id_states, source_id, id), 
         # Fix for OK should be back in OK processing...
         id = if_else(state == "OK", stringr::str_pad(source_id, 6, pad = "0", side = "left"), id)) 
  
if(nrow(dat) == nrow(original)) {
  saveRDS(dat, "data/linked_data.rds")
} else {
  stop("Rows dont match!")
}
