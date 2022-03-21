source("libs_and_paths.R")

# collected by a transparency org

setwd(paste0(stubs$dh,"Nevada/Payroll/"))

list <- list.files(full.names=TRUE, pattern = "*.csv", recursive=TRUE) 
read <- lapply(list, read_csv, col_types = "cccccccccccc")
df <- read%>%  bind_rows()

# ~half the records have name as 'last, first middle' the rest are "first middle last"

print <- FALSE
df2 <- df %>% 
  mutate(hasComma = str_detect(`Employee Name`, ","),
         fun = ifelse(hasComma, "lcfm", "fml")) %>% 
  rowwise() %>%
  mutate( temp = ifelse(fun == "lcfm", 
                        clean_names_lcfm(`Employee Name`,suffix=suffix, print=print),
                        clean_names_fml(`Employee Name`,suffix=suffix, print=print))) %>%
  ungroup() %>% 
  separate(temp, into=c("name_first", "name_middle", "name_last", "name_other"), sep=",")

# status is always misisng
sum(is.na(df2$Status))/nrow(df2)

df3 <- df2 %>% 
  select(c(starts_with("name_"), 
           position=`Job Title`,
           salary_base = `Base Pay`, 
           salary_overtime = `Overtime Pay`,
           salary_other = `Other Pay`,
           salary_benefits = Benefits,
           salary_total = `Total Pay`,
           salary_total_benefits = `Total Pay & Benefits`,
           year = Year, 
           location = Agency)) %>% 
  mutate_at(vars(starts_with("salary")), funs(as.numeric)) %>% 
  mutate(year = as.numeric(year),
         location = tolower(location), 
         position = tolower(position),
         teacher1 = str_detect(position,"teach") |
           str_detect(position, "tchr"),
         teacher0 = str_detect(position,"sub") |
           str_detect(position,"assistant") |
           str_detect(position,"aide"),
         teacher = teacher1 == TRUE & teacher0 != TRUE) %>% 
  select(-c(teacher0, teacher1))

# NA introductions are 'aggregate' and 'Not Provided' for teh salary vars
test <- df2 %>% select(sal = `Base Pay`) %>% mutate(salary = as.numeric(sal)) %>% filter(is.na(salary)) %>% group_by(sal) %>% count()
# currently iding 10% as teachers 
sum(df3$teacher,na.rm=T)/nrow(df3)


### Merge on NCES
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="NV") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()


df4 <- df3 %>% 
  mutate(NCES_name = toupper(location)) %>% 
  left_join(nces, by = "NCES_name")
sum(!is.na(df4$localid),na.rm=T)/nrow(df4)

unmatched <- df4 %>% filter(is.na(localid)) %>% group_by(location) %>% count()
matched <- df4 %>% filter(!is.na(localid)) %>% group_by(location) %>% count()

df5 <- df4 %>% 
  mutate(source = "Transparent Nevada (non-govt)")

write_csv(df5,paste0(paths$payroll,"NV_Payroll.csv"))
