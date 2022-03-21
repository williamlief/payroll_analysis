setwd(paste0(stubs$dh,"Indiana/Payroll"))

df <- tibble()
for(i in 1:6) df <- df %>% bind_rows(read_excel("100r_2012_2017_schools.xlsx",sheet=i))

# split/clean up names - formatted as "Last, suffix, first mi"
  # but suffix is sometimes missing, so we will split into three parts
  # first part is last name, 
  # if two parts: second part is "first mi" 
  # if three parts: second part is suffix, third part is "first mi"

df2 <- df %>% 
  mutate(employee_raw = employee) %>% 
  separate(employee, into = c("name_last", "p2", "p3"), sep = ", ") %>% 
  mutate(fm = case_when(is.na(p3)~p2,
                        !is.na(p3)~p3),
         fm = str_replace_all(fm, "  ", " "),
         name_other = case_when(!is.na(p3)~p2),
         fm = gsub(" S R", " SR", fm),
         fm = gsub(" J R", " JR", fm)) %>% 
  filter(!(grepl("ESTATE OF", fm, ignore.case=TRUE))) %>% 
  separate(fm, into=c("name_first", "nm1", "nm2", "nm3", "nm4"), sep = " ") %>% 
  mutate(name_middle = paste(nm1, nm2, nm3, nm4, sep=' '),
         name_middle = gsub("NA", '',name_middle)) %>% 
  select(-c(p2, p3, starts_with("nm")))

# Variable summaries
c.cnty <- df2 %>% group_by(cnty_description) %>% count()
c.dept <-  df2 %>% group_by(department) %>% count()
c.unit_id <- df2 %>% group_by(unit_id) %>% count()
c.unit_type <- df2 %>% group_by(unit_type_id) %>% count()
c.job_title <- df2 %>% mutate(position = tolower(job_title)) %>%  group_by(position) %>% count()
c.city <- df2 %>% group_by(business_city) %>% count()
c.job_title <- df3 %>% filter(teacher1 != 1) %>% mutate(position = tolower(job_title)) %>%  group_by(position) %>% count()
# what are unit ids?

# parse job titles (will need to use regext, 30k+ unique titels)
df3 <- df2 %>% 
  mutate(position = tolower(job_title),
    teacher1 = str_detect(position,"teach") |
      str_detect(position, "tch") |
      str_detect(position, "grade") |
      position %in% c("kindergarten","special education","elementary","social studies","language arts", 
                      "mathematics", "special ed", "grade 1", "grade 2", "grade 3", "grade 4", "grade 5",
                      "1st grade", "2nd grade", "3rd grade", "4th grade", "5th grade"),
    teacher0 = str_detect(position,"sub")|
      str_detect(position,"assis") |
      str_detect(position,"aid"), 
    teacher = teacher1 == TRUE & teacher0 != TRUE,
    NCES_name = toupper(department))


# merge on nces

nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="IN") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()
write.csv(nces,"IN_nces_xwalk.csv") # used for manual match

dists <- df3 %>% group_by(NCES_name, unit_id) %>% count() %>% arrange(NCES_name) # used for manual match
xwalk_raw <- read_csv("IN_nces_xwalk_edit.csv") %>% 
  arrange(localid) %>% 
  mutate_at(vars(starts_with("X")), 
            funs(str_replace_all(., "\xca", " "))) 

xwalk <- xwalk_raw %>% 
  select(-NCES_name, unit_id) %>% 
  filter(!is.na(X4) & X4!="" & X4 != "George and Victoria Phalen Leadership Academy") %>% 
  unique()
xwalk <- as.data.frame(xwalk)
nrow(xwalk %>% group_by(localid) %>% filter(max(row_number())>1)) == 0 # if this is false we will get duplication in the merge

xwalkl <- reshape(xwalk, 
                  idvar="localid",
                  varying=c("X4", "X5", "X6", "X7",
                            "X8", "X9", "X10", "X11",
                            "X12", "X13", "X14", "X15",
                            "X16"),
                  direction="long",
                  v.names = "X") %>% 
  filter(X!="") %>% 
  select(-time) %>% 
  mutate(localid = as.character(localid),
         localid = case_when(nchar(localid)==2~paste0("00",localid),
                             nchar(localid)==3~paste0("0", localid),
                             nchar(localid)==4~localid)) %>% 
  left_join(nces %>% select(-NCES_name, -NCES_type)) %>% 
  select(-unit_id) %>% 
  unique() %>% 
  filter(localid != 9770)

# Need to investigate these districts and see whats going on
test <- xwalkl %>% group_by(X) %>% filter(max(row_number())>1)

xwalkl2 <- xwalk_raw %>% 
  select(unit_id, localid) %>% 
  filter(!is.na(unit_id) & unit_id != "*") %>% 
  mutate(localid = as.character(localid),
         localid = case_when(nchar(localid)==2~paste0("00",localid),
                             nchar(localid)==3~paste0("0", localid),
                             nchar(localid)==4~localid)) %>% 
  left_join(nces %>% select(-NCES_name, -NCES_type)) %>% unique()

df4 <- df3 %>% rename(X="NCES_name") %>% 
  left_join(xwalkl, by = "X") %>% 
  left_join(xwalkl2, by = "unit_id") %>% 
  mutate(localid = ifelse(is.na(localid.x),localid.y,localid.x),
         NCES_leaid = ifelse(is.na(NCES_leaid.x),NCES_leaid.y,NCES_leaid.x),
         state = "IN")


sum(!is.na(df4$NCES_leaid))/nrow(df4)

df5 <- df4 %>% 
  select(state, year, salary=total_compensation, 
         name_first, name_middle, name_last, name_other,
         teacher, position, location=department, NCES_leaid, NCES_localid =localid) %>% 
  arrange(name_last, name_first, year) %>% 
  mutate_at(vars(starts_with("name_")),
                 funs(toupper(.)))

write_csv(df5,paste0(paths$payroll,"IN_Payroll.csv"))








## This was an attempt to use string matching to link nnces to indiana, i got to about a 40% match, but started having some duplication problems
# eventually i decided to manually link nces districts to indiana names this code is now unused

# ldf <- list(df3, nces)
# for (dat in 1:2) {
#   ldf[[dat]] <- 
#     mutate(ldf[[dat]],
#            NCES_name2 = NCES_name,
#            
#            NCES_name2 = str_replace(NCES_name2, "CORPORATION", "CORP"),
#            NCES_name2 = str_replace(NCES_name2, "CORP", "COR"),
#            NCES_name2 = str_replace(NCES_name2, "COMMUNITY", "COM"),
#            NCES_name2 = str_replace(NCES_name2, "COMM", "COM"),
#            NCES_name2 = str_replace(NCES_name2, "SCHOOLS", "SCH"),
#            NCES_name2 = str_replace(NCES_name2, "SCHOOL", "SCH"),
#            NCES_name2 = str_replace(NCES_name2, "SCHS", "SCH"),
#            NCES_name2 = str_replace(NCES_name2, "SCHLS", "SCH"),
#            NCES_name2 = str_replace(NCES_name2, "COUNTY", "CO"),
#            
#            NCES_name2 = str_replace(NCES_name2, "SCH",""),
#            NCES_name2 = str_replace(NCES_name2, "COR",""),
#            NCES_name2 = str_replace(NCES_name2, "COM",""),
#            NCES_name2 = str_replace(NCES_name2, "CO",""),
#            
#            NCES_name2 = str_replace_all(NCES_name2,"[[:punct:]]",""),
#            NCES_name2 = str_replace_all(NCES_name2," ",""),
#            
#            NCES_name2 = str_replace(NCES_name2, "TOWNSHIP", "TWP"),
#            NCES_name2 = str_replace(NCES_name2, "SERVICES", "SERVS"),
#            
#            NCES_name2 = str_replace(NCES_name2, "MSDOF", "MSD"),
#            NCES_name2 = str_replace(NCES_name2, "CONSOLID", "CONS"),
#            NCES_name2 = str_replace(NCES_name2, "CSCOF", ""),
#            NCES_name2 = str_replace(NCES_name2, "CSC", ""),
#            NCES_name2 = str_replace(NCES_name2, "CSD", ""),
#            
#            NCES_name2 = str_replace(NCES_name2, "USC", "UNITED"),
#            NCES_name2 = str_replace(NCES_name2, "CNT", "CENTRAL")
#     )
# }
# 
# ldf[[2]] <- ldf[[2]] %>% select(-NCES_name) %>% unique()
# test <- ldf[[2]] %>% group_by(NCES_name2) %>% count %>% arrange(desc(n))
# 
# 
# df4 <- ldf[[1]] %>% left_join(ldf[[2]], by = "NCES_name2") 
# sum(!is.na(df4$NCES_leaid))/nrow(df4)
# unmatched <- df4 %>% filter(is.na(NCES_leaid)) %>% group_by(NCES_name.x, NCES_name2) %>% count() %>% arrange(desc(n))
# unmatched2 <- ldf[[2]] %>% anti_join(ldf[[1]], by = "NCES_name2") 
# View(ldf[[2]])
# View(unmatched)
