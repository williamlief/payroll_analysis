setwd(paste0(stubs$dh,"Georgia/Payroll"))

df <- read_csv("SalaryTravelDataExportAllYears.csv")
# warnings are because there are trailing commas at the ends of the lines, so it looks like 7 variables, but the last one is empty, and the header row doesn't have the trailing comma

head(df)
# name is Last, First M 
title <- df %>% group_by(TITLE) %>% count() %>% arrange(desc(n))
head(title)
summary(df$SALARY) # UGH why are there negative salaries...
negsals <- df %>% filter(SALARY <0)

org <- df %>% group_by(ORGANIZATION) %>% count() %>% arrange(desc(n))
head(org) # probably want to pull all "... COUNTY BOARD OF EDUCATION"
table(df$FISCAL_YEAR)


suffs <- c("JR", "SR", "II", "III")


df2 <- df %>% 
  mutate(NAME = str_replace(NAME,"\xd1","N")) %>% 
  separate(.,NAME, c("name_last", "nm2.1", "nm2.2", "nm2.3"), sep=",") %>% 
  mutate_at(vars(name_last, nm2.1, nm2.2, nm2.3),
            funs(trimws(.))) %>% 
  mutate(name_other = case_when(nm2.1 %in% suffs~nm2.1,
                                nm2.2 %in% suffs~nm2.2),
         nm2.1 = ifelse(nm2.1 %in% suffs, "",nm2.1),
         nm2.2 = ifelse(nm2.2 %in% suffs, "",nm2.2),
         nm2 = gsub("NA", "", paste(nm2.1, nm2.2, nm2.3, sep=" "))) %>% 
  separate(., nm2, c("name_first", "nm3.1", "nm3.2", "nm3.3", "nm3.4", "nm3.5", "nm3.6"), sep = " ") %>% 
  mutate(name_middle = gsub("NA", "", paste(nm3.1, nm3.2, nm3.3, nm3.4, nm3.5, nm3.6, sep=" "))) %>% 
  select(-starts_with("nm"))

df3 <- df2 %>% 
  mutate(position = tolower(TITLE),
         teacher1 = str_detect(position,"teach") |
           str_detect(position, "tch") |
           str_detect(position, "grade"),
         teacher0 = str_detect(position,"sub") |
                    str_detect(position,"dispatcher") |
                    str_detect(position,"graduate teaching assistant"),
         teacher = teacher1 == TRUE & teacher0 != TRUE)

# Match to NCES
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="GA") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()

org2 <- org %>% 
  filter(str_detect(ORGANIZATION, "BOARD OF EDUCATION") |
           str_detect(ORGANIZATION, "SCHOOL")|
           str_detect(ORGANIZATION, "ACADEMY") |
           str_detect(ORGANIZATION, "KIPP") |
           str_detect(ORGANIZATION, "CHARTER")) %>% 
  mutate(NCES_name = gsub("BOARD OF EDUCATION", "",ORGANIZATION),
         NCES_name = gsub("SCHOOL SYSTEM", "",NCES_name),
         NCES_name = gsub("SCHOOL DISTRICT", "",NCES_name),
         NCES_name = gsub("ACADEMY", "",NCES_name),
         NCES_name = gsub("CHARTER", "",NCES_name),
         NCES_name = gsub("SCHOOL", "",NCES_name), 
         NCES_name = ifelse(grepl("CITY OF", NCES_name), paste(gsub("CITY OF", "",NCES_name),"CITY", sep=""),NCES_name),
         NCES_name = trimws(NCES_name),
         NCES_name = case_when(NCES_name == "ATLANTA INDEPENDENT"~ "ATLANTA PUBLIC SCHOOLS",
                               TRUE ~ NCES_name))

m1 <- org2 %>% 
  left_join(nces) %>% 
  mutate(merge = "t1")

m2 <- nces %>% 
  fuzzyjoin::regex_left_join(org2) %>% 
  rename(NCES_name = NCES_name.x) %>% 
  select(-NCES_name.y) %>% 
  mutate(merge = "t2")

m3 <- m1 %>% bind_rows(m2) %>% 
  group_by(ORGANIZATION) %>% 
  arrange(ORGANIZATION, NCES_leaid) %>% 
  filter(row_number() == 1)

unmatched <- m3 %>% filter(is.na(localid)|
                                is.na(ORGANIZATION))

df4 <- df3 %>% 
  left_join(m3) %>% 
  mutate_at(vars(starts_with("name_")), funs(trimws(.))) %>% 
  mutate(state = "GA") %>% 
  select(starts_with("name_"), salary = SALARY, position, location = ORGANIZATION, year = FISCAL_YEAR, teacher, 
         starts_with("NCES"), NCES_localid = localid, state)

write_csv(df4,paste0(paths$payroll,"GA_Payroll.csv"))
