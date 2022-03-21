setwd(paste0(stubs$dh,"Michigan/Payroll"))

# Files A-D are the main response, weirdly split across columns. Have to pray that the order was correctly presserved
files <- paste0("FOIA response ", c("A", "B", "C", "D"), ".xlsx")

read1 <- lapply(files, read_excel, col_types = "text")
file1 <- read1  %>% bind_cols
test <- file1 %>% group_by(job_class) %>% count() %>% arrange(desc(n))
# most common description is teaching, but only covers 108000 people.

# 2017-015-Response
foia_2017 <- read_excel("2017-015-Response.xlsx", col_types="text")
test <- foia_2017 %>% group_by(`Position Cd Desc`) %>% count() %>% arrange(desc(n))
# this file appears to be all sorts of non-teachers, covers 2014-2016, no deid

# 2017-03-27_foia_corrected
foia_delete <- read_excel("2017-03-27_foia_corrected.xlsx", col_types="text", sheet="deids to remove")
foia_insert <- read_excel("2017-03-27_foia_corrected.xlsx", col_types="text", sheet="records to insert")

# Combine
file2 <- file1 %>% 
  anti_join(foia_delete) %>% 
  bind_rows(foia_insert) 

# make long
file3 <- file2 %>% 
  gather(key=year,value=salary, starts_with("sum_wag_amt")) %>% 
  mutate(state = "MI",
         year = as.numeric(substr(year,13,16)),
         salary = as.numeric(salary),
         name_first=tolower(fst_nm),
         name_last =tolower(last_nm),
         position = tolower(job_class),
         location = toupper(org_nm),
         source_id = deid) %>% 
  select(year, name_first, name_last, position, location, salary, source_id)

# Pull in the name xwalk - this was manually created to match MI school names to NCES names
# was created by doing a first merge of the mi names to nces (about 50% match rate) then comparing unmatched names from MI to nces list of districts
xwalk <- read_excel("MI_namexwalk.xlsx") %>% rename(location=MI_name)
file4 <- file3 %>% 
  left_join(xwalk) %>% 
  mutate(NCES_name = ifelse(is.na(NCES_name),location,NCES_name))
           
# NCES data
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="MI") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()

file5 <- file4 %>% 
  left_join(nces,
            by=c("NCES_name"))

sum(!is.na(file5$state))/ length(file5$state)
test <- file5 %>% filter(is.na(state)) %>% group_by(NCES_name) %>% count() 

# get rid of people without a nces match, might lose a couple school districts, but also losing all the other civil servants
# DO NOT get rid of people with NCES match, want full file saved
# split middle names
file6 <-file5 %>% 
  filter(salary != 0) %>% 
  separate(name_first, into = c("name_first","name_middle1","name_middle2"), sep=" ") %>% 
  mutate(name_middle = paste(name_middle1, name_middle2, sep=' '),
         name_middle = gsub("NA", '',name_middle),
         source = "Mackinac Center (non-govt)",
         state = "MI") %>% 
  select(state, year, salary, name_first, name_middle, name_last,
         school_district=location, position, NCES_name, NCES_localid=localid, NCES_leaid, source) 
         
write_csv(file6,paste0(paths$payroll,"MI_Payroll.csv"))


## 
# Other files
# FOIA - MC - 98 seems to be just the sum salary across the three years
# Judges 2015-2016 CY Wages - data for judges, don't care