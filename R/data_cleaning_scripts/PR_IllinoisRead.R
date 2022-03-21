setwd(paste0(stubs$dh,"Illinois/Payroll/2002-2012"))
list1 <- list.files()
read1 <- lapply(list1, read_excel, col_types = "text")
file1 <- read1  %>% bind_rows

setwd(paste0(stubs$dh,"Illinois/Payroll/2013-2017"))
list2 <- list.files()
read2 <- lapply(list2, read_excel, col_types = "text")
file2 <- read2 %>% bind_rows

table(file1[[1]])
table(file2[[1]]) # note that the sample size jumps from file1 to file2 - reporting standards must be changing

table(file1$dst_st) # why are there employees in MI and OR?

names(file1)
names(file2)

# check documentation for what the year is exactly
# Read in first set of data - Teacher Service Record
clean_1 <- file1 %>% 
  rename(year = fy, 
         name_first = first_name,
         name_middle = mid_init,
         name_last = last_name,
         race = race_ethnicity_desc,
         school_name = sch_name,
         school_district = dst_name,
         degree = high_degre_desc,
         exp_district = dist_exp,
         exp_state = state_exp,
         exp_oos = out_of_state_exp, 
         position = pos_desc,
         employ_type = emply_desc) %>% 
  mutate(teacher = position %in% c("Elementary Teacher", "High School Teacher", "Junior High/Middle Teacher",
                                   "Kindergarten", "Special Education Teacher"),
         salary = as.numeric(salary),
         school_id = paste0(rcdt,sch_num),
         source = "IL Teacher Service Record") %>% 
  select(-c(race_ethnicity_cd, starts_with("tsr_status"), emply_type,
            dst_addr, dst_city, dst_st, dst_zip, dst_zip_plus4, location_cd,
            bacc_coll, bacc_coll_desc, adv_coll, adv_coll_desc,
            high_degre_cd, 
            sch_addr, sch_city, sch_st, sch_zip, sch_zip_plus4,
            low_grade, low_grd_desc, high_grade, high_grd_desc, pos_cd, starts_with("assign"))) %>% 
  select(year, salary, name_first, name_middle, name_last, school_district, teacher, position, 
         school_id, school_name, gender, race, location_desc, employ_type, months_employed, pct_emp, fte, pct_admin,
         exp_district, exp_state, exp_oos, degree, source)
         
# Read in second set of data, Employment Information System
clean_2 <- file2 %>%  
  rename(year = `School Year`,
         source_id = `ID Number`,
         race = `Race/Ethnicity`,
         gender = Gender,
         position = Position,
         school_district = Employer,
         school_name = `Primary Working Location Name`,
         school_id = `Primary Working Location RCDTS`,
         fte = `Full Time Equivalent`) %>% 
  mutate(name_first  = tolower(`First name`),
         name_middle = tolower(`Middle Name`),
         name_last   = tolower(`Last Name`),
         salary = as.numeric(Salary),
         bonus = as.numeric(Bonus),
         benefits = as.numeric(`Retirement Benefits`) + as.numeric(`Other Benefits`) + as.numeric(`Annuities`),
         teacher = str_detect(toupper(position),"TEACHER"),
         fte = as.character(as.numeric(fte)*100),
         source = "IL Employment Information System") %>% 
  filter(`Retired Staff` %in% c("No", "0")) %>% 
select(year, salary, bonus, benefits, name_first, name_middle, name_last, source_id, school_district, teacher, position,
       school_id, school_name, gender, race, fte, source)

# Combine data     
clean <- clean_1 %>% bind_rows(clean_2) %>% 
  mutate(gender = case_when(gender %in% c("F","Female") ~ "F",
                            gender %in% c("M","Male") ~ "M"),
         race = case_when(race %in% c("American Indian or Alaska Native", "American Indian or Alaskan Native")~"AiAn",
                          race %in% c("Asian", "Asian or Pacific Islander", "Native Hawaiian or Other Pacific Islander")~"Asian",
                          race %in% c("Black or African American","Black, Non-Hispanic")~"Black",
                          race %in% c("Hispanic","Hispanic or Latino")~"Hispanic",
                          race %in% c("NULL","Unknow", "Unknown")~"Unknown", 
                          race %in% c("Two or More Races") ~ "Multi", 
                          race %in% c("White", "White, Non-Hispanic")~"White"),
         name_last = tolower(name_last),
         name_first = tolower(name_first),
         name_middle = tolower(name_middle)) %>% 
  arrange(name_last, name_first, year)

# Merge on NCES district IDs
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="IL") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year, -localid) %>% 
  unique()

clean2 <- clean %>% 
  mutate(NCES_name = toupper(school_district)) %>% 
  left_join(nces) %>% 
  mutate(state = "IL")

write_csv(clean2,paste0(paths$payroll,"IL_Payroll.csv"))



