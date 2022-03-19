library(fastLink)
library(tidyverse)
library(collapse)

raw <- readRDS("data-raw/clean_payroll_records.RDS") %>% 
  # CASE CORRECTION NEEDS TO BE MOVED TO DATA CLEANING
  mutate(across(c(name_first, name_middle, name_last, name_other, position), tolower))
dir <- "data/deterministic_fastLink"



# Functions ---------------------------------------------------------------

# These are not yet ready to be run independently and access vars in the global env

#' join two data frames, 1:1 matches only 
#'
#' @description Used to join by a list of variables, keep cases that are 1:1 matches. 
#' Used inside match_wrap
#'  
#' @details this should be refactored to be more performant. But its 
#' a simple step that joins two data frames and throws out any cases where 
#' more than one match was found. 
#'
#' @param criteria a character list of variables to match on
#' @param d1 dataframe 1 must have a variable `index1` that uniquely identifies 
#' each row
#' @param d2 dataframe 2 must have a variable `index2` that uniquely identifies 
#' each row
#'
#' @return a dataframe with two columns `index1`, `index2` that contains all 1:1 matching rows in d1 and d2
match_func <- function(criteria, d1, d2) {
  m <- d1 %>% 
    inner_join(
      d2 %>% select(all_of(criteria), index2), 
      by = criteria) %>% 
    group_by(!!!syms(criteria)) %>% 
    filter(n() == 1) %>% 
    ungroup() %>% 
    select(index1, index2)
  
  m 
}

#' Iterate matching through criteria list
#' 
#' @description Requires global vars y (an int) and state (a char string)
#' 
#' @details the function iterates through the variables in the criteria list. 
#' At each step it applies `match_func` and finds the 1:1 matches. Then it removes
#' those records from d1 and d2 using the variables index1 and index2` and 
#' applies the next step of matching. 
#'
#' @param criteria_list a list of variable sets to match on
#' @param d1 dataframe 1 must have a variable `index1` that uniquely identifies 
#' each row - CANNOT overlap with index2
#' @param d2 dataframe 2 must have a variable `index2` that uniquely identifies 
#' each row - CANNOT overlap with index1
#'
#' @return a dataframe with two columns `id` that is a unique identifier
#' for the match. id embeds the state and y values, the criteria index, and an
#' arbitrary row index, all `_` separated.
#' `index` is the original index value.
#' There will be two rows for each `id` value containing all 1:1 matching
#'  rows in d1 and d2. Any unmatched or 1/m/m:m/1/m rows are excluded.
match_wrap <- function(criteria_list, d1, d2, state, y) {
  
  matches <- vector(mode = "list", length = length(criteria_list))
  
  for (c in 1:length(criteria_list)) {
    matches[[c]] <- match_func(criteria_list[[c]], d1, d2)
    d1 <- d1 %>% anti_join(matches[[c]], by = "index1")
    d2 <- d2 %>% anti_join(matches[[c]], by = "index2")
  }
  
  matches <- bind_rows(matches, .id = "criteria_number") %>% 
    ungroup()
  
  matches <- matches %>% 
    mutate(id = paste(state, y, criteria_number, row_number(), sep = "_")) %>% 
    select(id, index1, index2)
  
  print(paste0("match_wrap found this many matches:", nrow(matches)))
  
  # m now has one row for each original record - use index to match back to d
  m <- bind_rows(
    matches %>% select(id, index1),
    matches %>% select(id, index2)
  ) %>% 
    mutate(index = coalesce(index1, index2)) %>% 
    select(id, index)
  
}


#' fastLink backstop 
#' 
#' @description Requires global vars y (an int) and state (a char string)
#' 
#' @details probabilistic matching on just first and last name, to be used after
#' match_wrap. Very computationally intensive. TODO: generalize the parameters
#'
#' @param d1 dataframe 1 must have a variable `index1` that uniquely identifies 
#' each row - CANNOT overlap with index2
#' @param d2 dataframe 2 must have a variable `index2` that uniquely identifies 
#' each row - CANNOT overlap with index1
#' @param fl.stringdist.match var names to string match
#' @param fl.stringdist.match var names to allow partial matching
#' 
#' @return a dataframe with two columns `id` that is a unique identifier
#' for the match and index. id embeds the state and y values, "fastlink", and an
#' arbitrary row index, all `_` separated.
#' `index` is the original index value.
#' There will be two rows for each `id` value containing all 1:1 matching
#'  rows in d1 and d2. Any unmatched or 1/m/m:m/1/m rows are excluded.
backstop_fastLink <- function(d1, d2, 
                              fl.stringdist.match = c("name_first", "name_last"),
                              fl.partial.match = c("name_first", "name_last"), 
                              n.cores = 31, 
                              state, y) {
  
  # This section should be improved! But I don't want to overfit to Oklahoma

  # only match on the year pair, exclude prior years so fastLink doesn't get bogged down
  d1 <- d1 %>% filter(year == !!y) 
  
  print(paste0("Running backstop fastlink on ",
               nrow(d1), ":", nrow(d2), " records"))
  
  fl.varnames = unique(c(fl.stringdist.match, fl.partial.match))
  
  matches.out <- fastLink(
    dfA = d1,
    dfB = d2,
    varnames = fl.varnames,
    stringdist.match = fl.stringdist.match,
    partial.match = fl.partial.match,
    cut.a = 0.92, cut.p = 0.88,
    dedupe.matches = TRUE,
    # estimate.only = TRUE,
    n.cores = n.cores,
    threshold.match = 0.80,
    verbose = FALSE
  )
  
  fl.matches <- getMatches(
    dfA = d1, dfB = d2,
    fl.out = matches.out,
    threshold.match = 0.80,
    combine.dfs = FALSE
  )
  
  fl.m <- fl.matches %>%
    map(rownames_to_column) %>% #rowname is an index of the match in d1 and an index in d2 - NOT the same as row_number() used below in my id
    map(mutate, id = paste(state, y, "fastlink", row_number(), sep = "_")) %>% # THIS MUST BE PRE bind_rows - fastlink getMatches created two dataframes that are linked by row number (NOT by rowname!)
    bind_rows() %>% 
    select(index1, index2, id)
  
  # additional duplicate removal
  fl.m1 <- fl.m %>% filter(!is.na(index1)) %>%  group_by(index1) %>% filter(n() == 1)
  fl.m2 <- fl.m %>% filter(!is.na(index2)) %>%  group_by(index2) %>% filter(n() == 1)
  
  fl.m.dedup <- bind_rows(fl.m1, fl.m2) %>% 
    group_by(id) %>% 
    filter(n() == 2) %>% 
    ungroup()
  print(paste0("fastlink found this many matches:", nrow(fl.m.dedup)/2))
  
  fl.final <- fl.m.dedup %>% 
    mutate(index = coalesce(index1, index2)) %>% 
    select(id, index)
}


#' ensemble link a year pair
#' 
#' @description wrapper for match_wrap and backstop_fastlink
#' 
#' @details pulls data from y+1 and =< y where not id'd for matching. Runs the 
#' deterministic match and then a backstop fastlink (which can be turned off). 
#' Designed to be run inside an iterator function
#'
#' @param d data frame with administrative records - must have variables: 
#' index: that is unique row identifier 
#' ensemble_id: will be updated with successive calls, null on first call
#' year: var to identfiy the year
#' all vars that are used by criteria_list and backstop_fastlink
#' @param y - int for year
#' @param state character string indicating state e.g. "PA"
#' @param fastlink boolean to run backstop fastlink or not
#'
#' @return
#' @export
#'
#' @examples
ensemble_linkage <- function(d, y, state, fastlink) {
  # Get this year and unmatched previous years
  d1 <- d %>% 
    filter(year == (!!y) |
             (year < !!y & is.na(ensemble_id))) %>% 
    rename(index1 = index)
  print(d1 %>% count(year))
  
  # Get next year
  d2 <- d %>% 
    filter(year == !!y+1) %>% 
    rename(index2 = index)
  print(d2 %>% count(year))
  
  #TODO: move the criteria list out as a parameter to the outer function
  m <- match_wrap(criteria_list = 
                    list(
                      c("name_first", "name_middle", "name_other", "name_last", "position", "NCES_leaid"),
                      c("name_first", "name_last", "position", "NCES_leaid"),
                      c("name_first", "name_last", "position"),
                      c("name_first", "name_last", "teacher"),
                      c("name_first", "name_last", "NCES_leaid"),
                      c("name_first", "name_last"),
                      c("name_first", "round_salary", "position", "NCES_leaid")), 
                  d1 = d1, d2 = d2, 
                  state = state, y = y)
  
  if(fastlink) {
    fl <- backstop_fastLink(
      # remember d1 and d2 are at original length, reduction only happened within match_wrap
      # we only want to apply fastlink to the records that match_wrap couldnt link deterministically
      d1 = d1 %>% anti_join(m %>% select(index1 = index), by = "index1"),
      d2 = d2 %>% anti_join(m %>% select(index2 = index), by = "index2"), 
      state = state, y = y)
    
    # combine m
    m <- bind_rows(m, fl)
    
  }
  
  # Now update d to add on the matches
  m <- rename(m, newid = id)
  
  d <- d %>% 
    left_join(m, by = "index")
  
  # Combine the new matches with earlier matches and keep the earliest found
  
  # group by this rounds id, then downup fill to grab the permenant id - this links across years
  d_na <- d %>% filter(is.na(newid)) # exclude those who werent linked this round
  d_not_na <- d %>% 
    filter(!is.na(newid)) %>% 
    group_by(newid) %>% 
    tidyr::fill(ensemble_id, .direction = "downup") %>% 
    ungroup()
  d <- bind_rows(d_na, d_not_na) %>% 
    mutate(ensemble_id = coalesce(ensemble_id, newid)) 
  
  # preserve this rounds id for debugging
  d[[paste0("ensemble_id_", y)]] <- d$newid
  d <- d %>% select(-newid)
  
  d
}

#' Iterative Year-Pair Linking
#' 
#' @description Compares records within a state year by year using a series of 
#' exact matching criteria and a probabilistic backstop
#' 
#' @details
#'
#' @param state character string indicating state e.g. "PA"
#' @param dat data frame with administrative records. 
#' @param fastlink boolean to run backstop fastlink or not
#'
#' @return
#' @export
#'
#' @examples
iterateDeterministic_fastLink <- function(state, dat, fastlink = TRUE) {
  
  # salary manipulation should be moved out
  # leaving state subset in, someday allow for cross state linkage?
  salary_round = 5000 
  dat <- dat %>% filter(state == !!state) %>% 
    mutate(round_salary = round(salary/!!salary_round) * !!salary_round) %>% 
    mutate(index = row_number()) 
  
  years <- sort(unique(dat$year))
  
  # will be overwriting d, split it from dat for easier debugging
  d <- dat %>% 
    mutate(ensemble_id = NA)
  
  for ( y in years) {
    print("---------------------")
    print(y)
    if(y == max(years)) {
      print(paste("final year", y, "exiting loop"))
      break  }
    print(paste("linking to", y+1))
    
    d <- ensemble_linkage(d = d, y = y, state = state, fastlink = fastlink)
  }
  
  
  # After iterating through all years, fill in an arbitrary id value
  d_final <- d %>% 
    mutate(ensemble_id = if_else(is.na(ensemble_id), 
                          paste(state, year, "nomatch", row_number(), sep = "_"),
                          ensemble_id))
  
  return(d_final)
} 


# Test with sample data 
# Sample districts for testing 
set.seed(351129)
sample_districts <- raw %>% filter(state == "OK") %>% 
  select(NCES_leaid) %>% 
  sample_n(5)
testdat <- raw %>% 
  filter(NCES_leaid %in% sample_districts$NCES_leaid) 

test_ok <- iterateDeterministic_fastLink("OK", testdat)
saveRDS(test_ok, paste0(dir, "/testok_ensemble.rds"))

# Iterate through states 
states <- unique(raw$state)
states <- states[!grepl("ND", states)] # remove ND, no vars for matching

sink(paste0(dir, "/log.txt"), append = "TRUE", type = c("output", "message"))
for (state in states) {
  print(paste("starting", state, Sys.time()))
  sink(paste0(dir, "/log_", state, ".txt"), append = "TRUE", type = c("output", "message"))
  print(paste("starting", state, Sys.time()))
  try({
    det_res <- iterateDeterministic_fastLink(state, raw %>% filter(state == !!state))
    saveRDS(det_res, paste0(dir, "/", state, ".RDS"))
  })
  sink()
  print(paste("finished", state, Sys.time()))
  gc()
}
sink()
