# PR_OhioRead
# Created by Lief Esbenshade

source("libs_and_paths.R")


# Read in Files ----------------------------------------------------------------
# pre-cleaned in Stata

df <- haven::read_dta("/Volumes/SSD/DataHouse/StateData/Ohio/Payroll/ohio_full.dta")

# Assign name based ID ---------------------------------------------------------
# 2007-2011 data has middle names in the first name
df <- df %>% 
  separate(name_first, into = c("name_first", "name_middle"), sep = " ", extr = "drop")

df$id <- df %>% group_indices(name_last, name_first)

# Reduce to one record per employee --------------------------------------------
# keep highest FTE, highest salary, arbitrary
# count number of dists, schools, positions

df2 <- df %>% 
  arrange(id, year, desc(days_worked), desc(salary)) %>% 
  group_by(id, year) %>% 
  mutate(
    n_districts = n_distinct(district, na.rm = TRUE),
    n_schools   = n_distinct(school, na.rm = TRUE),
    n_position  = n_distinct(position, na.rm = TRUE),
    salary = sum(salary, na.rm = TRUE)
  ) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# potentially worrying increase in number of districts per year
df2 %>% count(year, n_districts) %>% spread(year, n)


# Merge on NCES ----------------------------------------------------------------
# Ohio does not have unique district names, and unfortunately the payroll data 
# doesnt have district ids. In theory we could identify using school names 
# within districts, but thats another battle for another day. Here I use 
# merge_check to exclude all non-unique district names from the match. I end up
# not matching about ~9% of records to NCES. 

df2 <- df2 %>% 
  mutate(NCES_name = toupper(district),
         merge_check = 1)

nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv"), col_types = "dcccdc") %>% 
  filter(state=="OH") %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  mutate(NCES_name = toupper(NCES_name)) %>% 
  select(-c(year, state, NCES_type)) %>% 
  unique() %>% 
  group_by(NCES_name) %>% 
  mutate(merge_check = n()) %>% # district names are not unique in Ohio
  ungroup()

df3 <- df2 %>% 
  left_join(nces, by = c("NCES_name", "merge_check"))
if (nrow(df2) != nrow(df3)) warning("the nces merge inflated the observation count")

df3 %>% summarize(mean(is.na(NCES_leaid)))
na <- liefTools::summarize_na(df3, round=3)

# Finalize and save ------------------------------------------------------------

df3 <- df3 %>% 
  mutate(
    teacher = position %in% c("teacher assignment", "teacher", "regular teaching",
                              "special education teaching"),
    state = "OH",
    source = "OH_GOVT"
  )

write_csv(df3, paste0(paths$payroll, "OH_Payroll.csv"))