library(fastLink)
library(tidyverse)
library(collapse)

raw <- readRDS("data-raw/clean_payroll_records.RDS")
dir <- "data/deterministic"

iterateDeterministic <- function(state, dat) {
  
  salary_round = 5000
  dat <- dat %>% filter(state == !!state) %>% 
    mutate(round_salary = round(salary/!!salary_round) * !!salary_round)
  
  years <- sort(unique(dat$year))
  
  d <- dat %>% 
    mutate(detid = NA)
  
  for ( y in years) {
    print(y)
    if(y == max(years)) {
      print(paste("final year", y, "exiting loop"))
      break  }
    print(paste("linking to", y+1))
    
    
    d1 <- d %>% 
      filter(year == (y) |
               (year < y & is.na(detid))) 
    print(d1 %>% count(year))
    
    d2 <- d %>% 
      filter(year == y+1)
    print(d2 %>% count(year))
    
    d1 <- d1 %>% mutate(index1 = row_number())
    d2 <- d2 %>% mutate(index2 = row_number())
    
    match_func <- function(criteria, d1, d2) {
      m <- d1 %>% 
        tidylog::inner_join(d2 %>% select(all_of(criteria), index2), by = criteria) %>% 
        group_by(!!!syms(criteria)) %>% 
        tidylog::filter(n() == 1) 
      m
    }
    
    
    match_wrap <- function(criteria_list, d1, d2) {
      
      matches <- vector(mode = "list", length = length(criteria_list))
      
      for (c in 1:length(criteria_list)) {
        matches[[c]] <- match_func(criteria_list[[c]], d1, d2)
        d1 <- d1 %>% tidylog::anti_join(matches[[c]], by = "index1")
        d2 <- d2 %>% tidylog::anti_join(matches[[c]], by = "index2")
      }
      
      matches <- bind_rows(matches, .id = "criteria_number") %>% 
        ungroup() %>% 
        mutate("detid_{y}" := paste(state, y, criteria_number, row_number(), sep = "_")) %>% 
        select(-criteria_number)
      
      matches
    }
    
    # todo: pull the criteria list out as a parameter
    matches <- match_wrap(criteria_list = 
                            list(
                              c("name_first", "name_middle", "name_other", "name_last", "position", "NCES_leaid"),
                              c("name_first", "name_last", "position", "NCES_leaid"),
                              c("name_first", "name_last", "position"),
                              c("name_first", "name_last", "teacher"),
                              c("name_first", "name_last", "NCES_leaid"),
                              c("name_first", "name_last"),
                              c("name_first", "round_salary", "position", "NCES_leaid")), 
                          d1, d2)
    
    m <- bind_rows(
      inner_join(d1, matches %>% select(paste0("detid_",y), index1)),
      inner_join(d2, matches %>% select(paste0("detid_",y), index2))
    )
    
    print(nrow(m))
    
    d <- d %>% 
      tidylog::left_join(m %>% select(-c(index1, index2)))
    
    d$curdetid <- (d[, paste0("detid_", y)]) %>% pull()
    
    if ( y != min(years)) {
      d_na <- d %>% filter(is.na(curdetid))
      d_not_na <- d %>% 
        filter(!is.na(curdetid)) %>% 
        group_by(curdetid) %>% 
        tidyr::fill(detid, .direction = "downup") %>% 
        ungroup()
      d <- bind_rows(d_na, d_not_na)
    }
    
    d <- d %>% 
      mutate(detid = coalesce(detid, curdetid)) %>%
      select(-curdetid)
  }
  
  d_final <- d %>% 
    mutate(detid = if_else(is.na(detid), 
                          paste(state, year, row_number(), sep = "_"),
                          detid))
  
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

test_ok <- iterateDeterministic("OK", testdat)
saveRDS(test_ok, paste0(dir, "/testok_7step.rds"))


# check <- test_ok %>% count(source_id, detid)
# print("number of detids per source_id")
# check %>% count(source_id) %>% count(n)
# print("number of source_ids per detid")
# check %>% count(detid, sort = T) %>% count(n)


# Iterate through states 
states <- unique(raw$state)
states <- states[!grepl("ND", states)] # remove ND, no vars for matching

sink(paste0(dir, "/log.txt"), append = "TRUE", type = c("output", "message"))
for (state in states) {
  print(paste("starting", state, Sys.time()))
  sink(paste0(dir, "/log_", state, ".txt"), append = "TRUE", type = c("output", "message"))
  print(paste("starting", state, Sys.time()))
  try({
    det_res <- iterateDeterministic(state, raw %>% filter(state == !!state))
    saveRDS(det_res, paste0(dir, "/", state, ".RDS"))
  })
  sink()
  print(paste("finished", state, Sys.time()))
  gc()
}
sink()
