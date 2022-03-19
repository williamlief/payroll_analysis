library(fastLink)
library(tidyverse)
library(collapse)

raw <- readRDS("data-raw/clean_payroll_records.RDS") %>% 
  mutate(across(c(name_first, name_middle, name_last, name_other, position), tolower))

iterateFastLink <- function(state, dat) {
  
  # for testing when not running the whole function
  # dat <- testdat
  # state <- "OK"
  # y <- 2007
  #  ^
  
  dat <- dat %>% filter(state == !!state)
  
  # matching params - maybe district blocking and iterate?
  fl.stringdist.match = c("name_first", "name_last", "position")
  fl.partial.match = c("name_first", "name_last", "position")
  fl.numeric.match = c("salary")
  fl.varnames = unique(c(fl.stringdist.match, fl.partial.match, fl.numeric.match))
  
  years <- sort(unique(dat$year))
  
  d <- dat %>% 
    mutate(flid = NA)
  
  for ( y in years) {
    print(y)
    if(y == max(years)) {
      print(paste("final year", y, "exiting loop"))
      break  }
    print(paste("linking to", y+1))
    
    
    if(y == min(years)) {
      d1 <- d %>% 
        filter(year == y)
    } else { 
      d1 <- d %>% 
        filter(year == (y) |
                 (year < y & is.na(flid))) 
    }
    print(d1 %>% count(year))
    
    d2 <- d %>% 
      filter(year == y+1)
    print(d2 %>% count(year))
  
    matches.out <- fastLink(
      dfA = d1,
      dfB = d2,
      varnames = fl.varnames,
      stringdist.match = fl.stringdist.match,
      partial.match = fl.partial.match,
      numeric.match = fl.numeric.match,
      dedupe.matches = TRUE,
      # estimate.only = TRUE,
      n.cores = 31,
      verbose = TRUE
    )

    matches <- getMatches(
      dfA = d1, dfB = d2,
      fl.out = matches.out,
      threshold.match = 0.85,
      combine.dfs = FALSE
    )

    m <- matches %>%
      map(rownames_to_column) %>%
      map(mutate, "flid_{y}" := paste(state, y, row_number(), sep = "_")) %>%
      bind_rows() %>%
      filter(!str_detect(rowname, "\\."))  # duplicate match removal

    nrow(m)
    
    d <- d %>% 
      tidylog::left_join(m %>% 
                           rename("posterior_{y}" := posterior) %>% 
                           select(-c(starts_with("gamma"), rowname)))
    
    d$curflid <- (d[, paste0("flid_", y)]) %>% pull()
    
    if ( y != min(years)) {
      d_na <- d %>% filter(is.na(curflid))
      d_not_na <- d %>% 
        filter(!is.na(curflid)) %>% 
        group_by(curflid) %>% 
        # mutate(old_flid = flid) %>% 
        tidyr::fill(flid, .direction = "downup") %>% 
        ungroup()
      d <- bind_rows(d_na, d_not_na)
    }
    
    
    
    d <- d %>% 
      mutate(flid = coalesce(flid, curflid)) %>%
      select(-curflid)
    
  }
  
  d_final <- d %>% 
    mutate(flid = if_else(is.na(flid), 
                          paste(state, year, row_number(), sep = "_"),
                          flid))
  
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

test_ok <- iterateFastLink("OK", testdat)
saveRDS(test_ok, paste0("data/fastLink/testok.rds"))

# 
# check <- d %>% count(source_id, flid)
# print("number of flids per source_id")
# check %>% count(source_id) %>% count(n)
# print("number of source_ids per flid")
# check %>% count(flid, sort = T) %>% count(n)
# 

# Iterate through states 
states <- unique(raw$state)
states <- states[-1] # testing without california
states <- states[!grepl("ND", states)] # remove ND, no vars for matching

sink("data/fastLink/log.txt", append = "TRUE")
for (state in states) {
  print(paste("starting", state, Sys.time()))
  sink(paste0("data/fastLink/log_", state, ".txt"), append = "TRUE")
  print(paste("starting", state, Sys.time()))
  try({
    fl_res <- iterateFastLink(state, raw %>% filter(state == !!state))
    saveRDS(fl_res, paste0("data/fastLink/", state, ".RDS"))
  })
  sink()
  print(paste("finished", state, Sys.time()))
}
sink()




