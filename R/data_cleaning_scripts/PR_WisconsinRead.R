# PR_WisconsinRead
# Created by Lief Esbenshade
#
# This program reads in school district employee records from 1995-2018. 
# It selects a subset of all variables available to standardize, and merges on 
# NCES district identifiers. The final files is saved as a csv. 
# Files were downloaded from: 
#  https://dpi.wi.gov/cst/data-collections/staff/published-data
#  https://publicstaffreports.dpi.wi.gov/PubStaffReport/Public/PublicReport/AllStaffReport  

###
# Todo:
# confirm that id reduction is working properly
# Confirm all parsing failures are ok
# check nas coerced into file_txt with creation of year variable
# figure out what the ids mean exactly
# replaces funs with list in fillers function

source("libs_and_paths.R")

setwd(paste0(stubs$dh,"Wisconsin/Payroll/"))


# Read in Files ----------------------------------------------------------------

list <- list.files("Raw", full.names=TRUE) 

# read the csv files 2017, 2018
file_2017 <- read_csv("Raw/AllStaffReportPublic_2016_2017.csv", col_types = cols(.default = "c"), skip = 1) %>% mutate(year = 2017)
file_2018 <- read_csv("Raw/AllStaffReportPublic_2017_2018.csv", col_types = cols(.default = "c"), skip = 1) %>% mutate(year = 2018)

file_csv <- bind_rows(file_2017, file_2018)
rm(file_2017, file_2018)

names(file_csv) <- tolower(names(file_csv))
names(file_csv) <- str_replace_all(names(file_csv),"[[:punct:]]","")
names(file_csv) <- str_replace_all(names(file_csv)," ","_")

# read the excel files 2015, 2016
file_2015 <- read_xlsx("Raw/AllStaff2015rev10-31-2016.xlsx", sheet = "All Staff 2014-2015", skip = 3, col_types = "text") %>% 
  mutate(year=2015)
file_2016 <- read_xlsx("Raw/AllStaff2016rev11-01-16.xlsx", sheet = "All Staff 2015-2016", skip = 3, col_types = "text") %>% 
  mutate(year=2016)

file_excel <- bind_rows(file_2015,file_2016)
rm(file_2015, file_2016)

names(file_excel) <- tolower(names(file_excel))
names(file_excel) <- str_replace_all(names(file_excel),"[[:punct:]]","")
names(file_excel) <- str_replace_all(names(file_excel)," ","_")


# read the txt files, these are fwf
# I read through the documentation and found 5 clusters of formatting rules. 
# files following same fwf: 1995 - 1998, 1999 - 2003, 2004 - 2007, 2008-2011, 2012-2014
# I created an excel file that has the formatting rules for each year cluster
# Due to raggedness, I set the last length to NA
# 2012-2014 files have a leading quotation mark, adjusted the first width by 1 to allow for this
# 2004-2007 files filler at 132 should be length 14, not 15

fwf_formats <- read_excel("FWF_formats.xlsx") %>% 
  group_by(Year) %>% 
  mutate(Length = ifelse(row_number()==max(row_number()), NA, Length),
         Variable = tolower(Variable),
         Variable = str_replace_all(Variable,"[[:punct:]]",""),
         Variable = str_replace_all(Variable," ","_")) %>% 
  # adjustments
  mutate(Length = if_else(Year == 12 & Start == 1, 10, Length),
         Length = if_else(Year == 4  & Start == 132, 14, Length))

list.txt <- list[!str_detect(list,"xlsx|csv")]

read_wi_txt <- function(chunk) {
  fwf <- filter(fwf_formats, Year==chunk)

  if (chunk == 95) {list <- list.txt[str_detect(list.txt, paste(c(95:98), collapse="|"))]}
  if (chunk == 99) {list <- list.txt[str_detect(list.txt, paste(c("99", "00", "01", "02", "03"), collapse="|"))]}
  if (chunk == 04) {list <- list.txt[str_detect(list.txt, paste(c("04", "05", "06", "07"), collapse="|"))]}
  if (chunk == 08) {list <- list.txt[str_detect(list.txt, paste(c("08", "09", "10", "11"), collapse="|"))]}
  if (chunk == 12) {list <- list.txt[str_detect(list.txt, paste(c(12:14), collapse="|"))]}
  
  print(list)
  
  wi_txt_list <- lapply(list, read_fwf, fwf_widths(fwf$Length), col_types = cols(.default = "c"))

  wi_txt <- wi_txt_list %>% bind_rows()
  names(wi_txt) <- fwf$Variable
  wi_txt <- wi_txt %>%   setNames(make.names(names(.), unique = TRUE))

  return(wi_txt)
}

file_txt <- lapply(c(95,99,4,8,12), read_wi_txt) %>% bind_rows
# 95, 03, 04, 05, 06, 07, have 1000s of parsing failures
# 95's parsing failures are almost entirely at x60 or x61, and are due to the ragged end
# other files have few to none

# Add year, remove leading " for 2012:2014
file_txt <- file_txt %>%  
  mutate(
    year = as.numeric(substr(file_txt$year_and_session,1,4)),
    id_number = if_else(year %in% 2012:2014, substring(id_number, 2), id_number))

# Confirm filler vars are empty 
fillers <- file_txt %>% 
  select(year, starts_with("fille")) %>% 
  group_by(year) %>% 
  summarize_all(funs(sum(!is.na(.))/n()))

# Combine Files ----------------------------------------------------------------

df <- bind_rows(file_excel, file_txt, file_csv) 

rm(file_csv, file_excel, file_txt)

# Combine Variables
df <- df %>% 
  mutate(
    name_last  = tolower(last_name),
    name_first = tolower(name_first),
    gender = case_when(
      year %in% 1995:1998 ~ sex,
      year %in% 1999:2011 ~ sexgender,
      year %in% 2012:2014 ~ gender,
      year %in% 2015:2016 ~ gndr,
      year %in% 2017:2018 ~ gender),
    race = case_when(
      year %in% 1995:1998 ~ race,
      year %in% 1999:2011 ~ raceprimary_ethnic_origin,
      year %in% 2012:2014 ~ raceethnicity,
      year %in% 2015:2016 ~ raceethn,
      year %in% 2017:2018 ~ raceethnicity),
    degree = case_when(
      year %in% 1995:2011 ~ highest_degree,
      year %in% 2012:2014 ~ highest_degree_code,
      year %in% 2015:2016 ~ high_degree,
      year %in% 2017:2018 ~ substring(contract_high_degree,1,1)), 
    salary = case_when(
      year %in% 1995:2011 ~ salary,
      year %in% 2012:2014 ~ total_salary,
      year %in% 2015:2016 ~ tot_salary,
      year %in% 2017:2018 ~ total_salary),
    salary_fringe = case_when(
      year %in% 1995:2011 ~ fringe,
      year %in% 2012:2014 ~ total_fringe__employee_benefits,
      year %in% 2015:2016 ~ tot_fringe,
      year %in% 2017:2018 ~ total_fringe),
    experience_local = case_when(
      year %in% 1995:2014 ~ local_experience_in_education,
      year %in% 2015:2016 ~ local_exp,
      year %in% 2017:2018 ~ contract_local_experience),
    experience_total = case_when(
      year %in% 1995:2014 ~ total_experience_in_education,
      year %in% 2015:2016 ~ total_exp,
      year %in% 2017:2018 ~ contract_total_experience),
    position_code = case_when(
      year %in% 1995:2014 ~ position_code, 
      year %in% 2015:2016 ~ position_cd,
      year %in% 2017:2018 ~ substring(assignment_position,1,2)),
    localid = case_when(
      year %in% 1995:2011 ~ agency_of_work_location,
      year %in% 2012:2014 ~ agency_of_work_location_code,
      year %in% 2015:2016 ~ work_agncy_cd,
      year %in% 2017:2018 ~ substr(assignment_work_agency,1,4)),
    school_id = case_when(
      year %in% 1995:2012 ~ school_of_work_location,
      year %in% 2013:2014 ~ school_of_work_location_code,
      year %in% 2015:2016 ~ school_cd,
      year %in% 2017:2018 ~ substr(assignment_work_school,1,4)),
    source_id = case_when(
      year %in% 2015:2016 ~ file_number,
      year %in% 2017:2018 ~ entity_id),
    other_id = case_when(
      year %in% 1995:2014 ~ id_number,
      year %in% 2015:2016 ~ id_nbr,
      year %in% 2017:2018 ~ research_id),
    state = "WI", 
    source = "WI DOE"
    ) %>% 
  select(
    state, year, salary, salary_fringe, 
    name_first, name_last, source_id, other_id, 
    localid, position_code, source, 
    gender, race, degree, experience_local, experience_total, birth_year
  )


# pull in codes for positions, clean up race, add in degree titles
codes <- read_excel("variable_codes.xlsx")

position_title <- codes %>% 
  filter(Variable == "position_code") %>% 
  select("position_code" = Code, 
         "position_title" = Description) %>% 
  mutate(position_code = if_else(nchar(position_code) == 1, 
                                 paste0("0",position_code), position_code)) %>% 
  distinct() %>% 
  # code definition has changed
  filter(! position_code %in% c("88", "92"))

df <- df %>% 
  left_join(position_title,
            by = "position_code") %>% 
  mutate(
    teacher = position_code == "53",
    race_title = case_when(
      race %in% c("A", "A - Asian") ~ "Asian",
      race %in% c("B", "B - Black or African American") ~ "Black",
      race %in% c("H", "H - Hispanic/Latino") ~ "Hispanic",
      race %in% c("I", "I - American Indian or Alaska Native") ~ "AmericanIndian_AlaskaNative",
      race %in% c("P", "P - Native Hawaiin or Other Pacific Islander") ~ "NativeHawiaan_PacificIslander",
      race %in% c("T", "Two or More Races") ~ "TwoOrMore",
      race %in% c("W", "W - White") ~ "White"), 
    degree_title = case_when(
      degree == "2" ~ "HighSchoolDiploma",
      degree == "3" ~ "Associate",
      degree == "4" ~ "Bachelor",
      degree == "5" ~ "Masters",
      degree == "6" ~ "6Year",
      degree == "7" ~ "Doctorate",
      degree == "8" ~ "Other"),
    salary = str_remove(salary, "$"),
    salary_fringe = str_remove(salary_fringe, "$"),
  )

# Deal with multiple observations
  # I THINK THIS WORKS PROPERLY - need to confirm with extensive visual checks
df2 <- df %>% 
  arrange(year, other_id, desc(salary)) %>% 
  group_by(year, other_id) %>% 
  filter(row_number() == 1)

summaryNA <- df2 %>% 
  group_by(year) %>% 
  summarize_all(funs(round(mean(is.na(.)),2)))
# HIGH NA
## Salary 2004:2007, 2017, 2018
## Degree 2015:2018
## Experience 2015:2018



# Merge in NCES ----------------------------------------------------------------

nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv"), col_types = "dcccdc") %>% filter(state=="WI") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017 &  
           !(NCES_name %in% c("SURING PUBLILC SCHOOL DISTRICT", # localids with two names, always just slight variations
                              "SEEDS OF HEALTH INC AGENCY", 
                              "MILWAUKEE COLLEGE PREPARATORY SCHOOL -- 36TH STREET AGENCY",
                              "CEO LEADERSHIP ACADEMY AGENCY",
                              "MILWAUKEE COLLEGE PREP SCHOOL: LOLA ROWE NORTH CAMPUS AGENCY"))
         ) %>%
  select(-c(year, state, NCES_type)) %>% 
  unique() 

len <- nrow(df2)
df2 <- df2 %>% 
  left_join(nces, by = "localid")
if (nrow(df2) != len) warning("the nces merge inflated the observation count")

# Finalize and save ------------------------------------------------------------

df3 <- df2 %>% 
  select(-race) %>% 
  rename("race" = race_title)

write_csv(df3,paste0(paths$payroll,"WI_Payroll.csv"))

