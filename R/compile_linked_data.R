library(tidyverse)
options(collapse_mask = "manip")
library(collapse)

original <- readRDS("data-raw/clean_payroll_records.RDS")

source_id_states = c("OK", "ND")

files <- list.files(path = "data/deterministic/", pattern = "*.RDS", full.names = TRUE)

raw <- map_dfr(files, readRDS)
raw <- raw %>% dplyr::select(-starts_with("detid_"))

# Set the primary id that will be used in all analysis
dat <- raw %>% 
  bind_rows(original %>% filter(state == "ND")) %>% # ND didn't get linked because it only provided IDs
  mutate(id = detid, 
         id = if_else(state %in% !!source_id_states, source_id, id), # use source_id where available
         across(c(name_first, name_middle, name_last, name_other, position), tolower)) 
  
if(nrow(dat == nrow(original))) {
  saveRDS(dat, "data/linked_data.rds")
} else {
  stop("Rows got dropped!")
}