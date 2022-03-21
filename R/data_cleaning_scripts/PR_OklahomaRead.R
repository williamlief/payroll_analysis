# PR_OklahomaRead
# Created by Lief Esbenshade

source("setup.R")


# Read in Files ----------------------------------------------------------------

list <- list.files(path = paste0(paths$raw,"StateData/Oklahoma/Payroll"), 
                   pattern = ".xlsx", full.names=TRUE) 

readfun <- function(input_file) {
  print(input_file)
  df <- readxl::read_excel(input_file, skip = 1, col_types = "text") 
  df$year <- 2001 + as.numeric(substr(stringr::str_extract(input_file, regex("FY\\d\\d")), 3, 4))
    
  return(df)
}

files <- list %>% map_dfr(readfun)

# Combine files & variables-----------------------------------------------------

df <- files %>% 
  mutate(
    source_id = coalesce(TeacherNumber, `Teacher Number`),
    source_id_2 = staff_id,
    name_first = tolower(coalesce(fname, firstname)),
    name_last = tolower(coalesce(lname, lastname)),
    localid = paste("OK", co, dist, sep = "-"),
    school_id = paste(co, dist, site, sep = "-"),
    gender = tolower(gender),
    experience_total = coalesce(total_experience, tot_exper),
    fte = coalesce(fte, FTE),
    salary_base = as.numeric(base_salary), 
    salary_fringe = as.numeric(total_fringe),
    salary_other = as.numeric(coalesce(total_oth_salary, `Other pay`)),
    salary_extra_duty = as.numeric(coalesce(total_extra_duty, `extra_duty pay`)),
    reason_for_leaving_desc = coalesce(reason_for_leaving, Reason_for_Leaving),
    reason_for_leaving_code = coalesce(`Reason For Leaving  (RFL_code)`, rfl_code)
    ) %>% 
  rename(county_id = co, 
         position = jobdesc,
         position_code = jobcode,
         race_code = race,
         degree_code = degree,
         subject_code = subject
         ) %>% 
  select(year, source_id, source_id_2, name_first, name_last, 
         county_name, county_id, localid, district_name, school_id, school_name,
         race_code, race_desc, gender, degree_code, degree_desc,
         position, position_code, subject_code, subject_desc, 
         experience_total, fte, starts_with("salary"), 
         reason_for_leaving_desc, reason_for_leaving_code) 
  
# Reduce to one record per employee --------------------------------------------
# keep highest FTE, highest salary, arbitrary
# count number of dists, schools, positions, subjects
# get rid of TOTAL OTHER PAY, TOTAL EXTRA DUTY PAY so dont double count

df2 <- df %>% 
  filter(!(position %in% c("TOTAL OTHER PAY", "TOTAL EXTRA DUTY PAY"))) %>% 
  arrange(source_id, year, desc(fte), desc(salary_base)) %>% 
  group_by(source_id, year) %>% 
  mutate(
    n_districts = n_distinct(localid, na.rm = TRUE),
    n_schools   = n_distinct(school_name, na.rm = TRUE),
    n_position  = n_distinct(position, na.rm = TRUE),
    n_subject   = n_distinct(subject_code, na.rm = TRUE),
    salary_base = sum(salary_base, na.rm = TRUE),
    salary_fringe = sum(salary_fringe, na.rm = TRUE),
    salary_other = sum(salary_other, na.rm = TRUE),
    salary_extra_duty = sum(salary_extra_duty, na.rm = TRUE)
  ) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# Merge on NCES ----------------------------------------------------------------
  
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv"), col_types = "dcccdc") %>% 
  filter(state=="OK") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  select(-c(year, state, NCES_type)) %>% 
  unique() 

df3 <- df2 %>% 
  left_join(nces, by = "localid")
if (nrow(df2) != nrow(df3)) warning("the nces merge inflated the observation count")

df3 %>% summarize(mean(is.na(NCES_leaid)))
na <- liefTools::summarize_na(df3, round=3)

# Finalize and save ------------------------------------------------------------

df3 <- df3 %>% 
  mutate(teacher = position %in% c("TEACHER", "RESOURCE TEACHER"),
         state = "OK",
         source = "OK_DOE")

write_csv(df3, paste0(paths$payroll, "OK_Payroll.csv"))