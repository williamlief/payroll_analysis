source("libs_and_paths.R")

setwd(paste0(stubs$dh,"Kansas/Payroll"))

# Kansas has 286 school districts, this data set only has 23...
# This data is a geographically representative sample

df <- read_csv("District Payroll.csv") %>% 
  rename(year = Year,
         name_last  = `Last Name`) %>% 
  separate(`First Name`, into = c("name_first","name_middle1","name_middle2"), sep=" ") %>% 
  mutate(name_middle = paste(name_middle1, name_middle2, sep=' '),
         name_middle = gsub("NA", '',name_middle),
         NCES_name = toupper(District),
         salary = (gsub('[$,]', '', `Total Pay`)))

positions <- df %>% group_by(Position) %>% count() %>% 
  mutate(teacher1 = 
           str_detect(toupper(Position),"TEACHER") |
           str_detect(toupper(Position),"TCH") | 
           Position %in% c("English", "Science", "Mathematics", "Social Studies", "Kindergarten-All Day", 
                           "First Grade", "Second Grade", "Third Grade", "Fourth Grade", "Fifth Grade", 
                           "ESOL", "Math", "Instructor", "Interrelated-Sr High", "Art", "Interrelated-Middle", 
                           "Vocal Music Elem", "Physical Ed - Elem", "Physical Education", "Physical Ed", 
                           "Social Science", "Certified Instructor", "Art-Elementary", "Kindergarten",
                           "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8"),
         teacher0 = 
           str_detect(toupper(Position), "AIDE") |
           str_detect(toupper(Position), "SUB") |
           str_detect(toupper(Position), "ASSISTANT") |
           str_detect(toupper(Position), "KITCHEN") |
           str_detect(toupper(Position), "RETIRED"),
         teacher = teacher1 == TRUE & teacher0 != TRUE)

# Less than a thid of employees are currently being coded as teachers
positions %>% group_by(teacher) %>% summarize(sum(n))

df <- df %>% 
  left_join(positions %>% select(Position,teacher))


nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="KS") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()

# merge was creating duplicates - two Blue Valley districts
  test <- nces %>% filter(toupper(NCES_name)=="BLUE VALLEY")
  # There are two blue valley school districts in Kansas
  # Johnson County has 1487 teachers id KS-D0229 and 2012000
  # Riley County   has 21   teachers id KS-D0384 and 2010980
  test2 <- df %>% filter(toupper(NCES_name) == "BLUE VALLEY") 
  test2 %>% group_by(year, teacher) %>% count()
  # 4000 records per year - also not being coded as teachers, using weird positions, is report 4,243 different position titles
  # its probably Johnson County Blue valley
  # crude fix is to just drop Riley County from the NCES file and match - if i ever get Riley county data will need to come up with a better solution

df2 <- df %>% 
  left_join(nces %>% 
              filter(NCES_leaid != "2010980") %>% 
              mutate(NCES_name = str_replace(NCES_name, "COLBY PUBLIC SCHOOLS", "COLBY"),
                     NCES_name = str_replace(NCES_name, "HUTCHINSON PUBLIC SCHOOLS", "HUTCHINSON"),
                     NCES_name = str_replace(NCES_name, "RILEY COUNTY", "RILEY"),
                     NCES_name = str_replace(NCES_name, "SHAWNEE MISSION PUB SCH", "SHAWNEE MISSION"),
                     NCES_name = str_replace(NCES_name, "TOPEKA PUBLIC SCHOOLS", "TOPEKA")),
            by=c("NCES_name"))

# check for non matches
df2 %>% filter(is.na(state)) %>% group_by(NCES_name, year) %>% count() 

df2 <- df2 %>% select(-c(`USD#`)) %>% 
  select(state, year, salary, name_first, name_middle, name_last, school_district=District, teacher, position=Position, 
         NCES_name, NCES_localid=localid, NCES_leaid) %>% 
  mutate(source = "KS Open Gov (non-govt)")

write_csv(df2,paste0(paths$payroll,"KS_Payroll.csv"))
