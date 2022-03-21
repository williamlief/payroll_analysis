source("libs_and_paths.R")

# PA data was previously cleaned in Stata. This file reads in the .dta file and makes some edits to match it to naming standards for this project

df <- read_dta("/Users/williamlief/Documents/Research Projects/Pennsylvania/Clean/pa_teach_db.dta")

df2 <- df %>% 
  rename("name_other" = name_suffix,
         "position" = position_description) %>% 
  mutate(NCES_name = toupper(lea_name), 
         teacher = position %in% c("Elementary", "Elementary Special Ed. Teacher", "Elementary Teacher",
                                   "Secondary", "Secondary Special Ed. Teacher", "Secondary Teacher", "Special Education",
                                   "Specialized, K-12 OR Middle School", "Speech Correctionist", "Ungraded Special Ed. Teacher",
                                   "Ungraded Teacher"),
         source = "PA DOE")

# NCES
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="PA") %>% 
mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()
         
df3 <- df2 %>% 
  left_join(nces) %>% 
  mutate(state = "PA")
test <- df3 %>% filter(is.na(NCES_leaid)) %>% group_by(lea_name) %>% count()

write_csv(df3,paste0(paths$payroll,"PA_Payroll.csv"))
