source("libs_and_paths.R")

# This is a subset of california districts collected by a transparency org
setwd(paste0(stubs$dh,"California/Payroll/TransparentCA"))

list <- list.files("FileExtract/", full.names=TRUE, pattern = "*.csv") 
read <- lapply(list, read_csv, col_types = "cccccccccccc")

df <- read%>%  bind_rows()
names(df) 

# this is imperfect, names that are "fml, suffix" currently being parsed as lcfm
ptm <- proc.time()
print <- FALSE # set to TRUE to help with debugging
df2 <- df %>% 
  mutate(hasComma = str_detect(`Employee Name`, ","),
         fun = ifelse(hasComma, "lcfm", "fml")) %>% 
  rowwise() %>%
  mutate( temp = ifelse(fun == "lcfm", 
                        clean_names_lcfm(`Employee Name`,suffix=suffix, print=print),
                        clean_names_fml(`Employee Name`,suffix=suffix, print=print))) %>%
  separate(temp, into=c("name_first", "name_middle", "name_last", "name_other"), sep=",") %>% 
  ungroup()
proc.time() - ptm

# status is misisng most of the time
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
# currently iding 29% as teachers
sum(df3$teacher,na.rm=T)/nrow(df3)


### Merge on NCES
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="CA") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()
# The same name is linking to multiple types and multiple ids...
nces_test <- nces %>% group_by(NCES_name) %>% count() %>%  arrange(desc(n)) %>% left_join(nces)

# for now workaround take the latest NCES_leaid (that doesnt fix)
# so also drop anything thats duplicated
# UGH - some duplication matters, see Jefferson Elementary SD, there's one in San Benito County, 
# one in San Joaquin county, and one in San Mateo County
# TCA file names dont include the county, to fix this I will need to go back and modify the download
# process to include the county in the file name. This is not the only case, added todo to trello
nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="CA") %>% 
  mutate(NCES_name = toupper(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  group_by(NCES_name) %>% 
  mutate(max = max(year),
         min = min(NCES_type)) %>% 
  ungroup() %>% 
  filter(year==max & NCES_type == min) %>% 
  unique()
nces <- nces %>% group_by(NCES_name) %>% count() %>%  arrange(desc(n)) %>% left_join(nces) %>% filter(n < 2) %>% select(-c(n,max,min, year)) %>% unique()

df4 <- df3 %>%
  mutate(NCES_name = toupper(location),
         NCES_name = str_replace(NCES_name, "CITY SCHOOL DISTRICT", "CITY"),
         NCES_name = str_replace(NCES_name, "UNION SCHOOL DISTRICT", "UNION"),
         NCES_name = str_replace(NCES_name, "UNIFIED SCHOOL DISTRICT", "UNIFIED"),
         NCES_name = case_when(
           NCES_name == "ACKERMAN CHARTER SCHOOL DISTRICT" ~ "ACKERMAN CHARTER",
           NCES_name == "ALBANY UNIFIED" ~ "ALBANY CITY UNIFIED",
           NCES_name == "AROMAS-SAN JUAN UNIFIED" ~ "AROMAS/SAN JUAN UNF",
           NCES_name == "BERRYESSA UNION" ~ "BERRYESSA UNION ELEM",
           NCES_name == "BONSALL UNION" ~ "BONSALL UNIFIED",
           NCES_name == "BRAWLEY ELEMENTARY SCHOOL DISTRICT" ~ "BRAWLEY ELEMENTARY",
           NCES_name == "BUCKEYE UNION" ~ "BUCKEYE UNION ELEM",
           NCES_name == "CAMBRIAN SCHOOL DISTRICT" ~ "CAMBRIAN",
           NCES_name == "CAMINO UNION" ~ "CAMINO UNION ELEM",
           NCES_name == "COLUMBIA UNION ELEMENTARY" ~ "COLUMBIA ELEMENTARY",
           NCES_name == "DEL NORTE COUNTY UNIFIED SCHOOLS" ~ "DEL NORTE COUNTY UNIFIED",
           NCES_name == "EASTSIDE UNION" ~ "EASTSIDE UNION ELEM",
           NCES_name == "EL DORADO UNION HIGH SCHOOL DISTRICT" ~ "EL DORADO UNION HIGH",
           NCES_name == "FRUITVALE SCHOOL DISTRICT" ~ "FRUITVALE ELEM",
           NCES_name == "GOLD TRAIL UNION" ~ "GOLD TRAIL UNION ELEM",
           NCES_name == "HARMONY UNION" ~ "HARMONY UNION ELEM",
           NCES_name == "HAWTHORNE SCHOOL DISTRICT" ~ "HAWTHORNE",
           NCES_name == "HOLLISTER ELEMENTARY" ~ "HOLLISTER",
           NCES_name == "IGO-ONO-PLATINA UNION ELEMENTARY" ~ "IGO ONO PLATINA UNION ELEMENTARY",
           NCES_name == "INDIAN DIGGINGS SCHOOL DISTRICT" ~ "INDIAN DIGGINS ELEM", 
           NCES_name == "JUNCTION ELEMENTARY" ~ "JUNCTION CITY ELEMENTARY", 
           NCES_name == "KENWOOD SCHOOL DISTRICT" ~ "KENWOOD",
           NCES_name == "KERN COUNTY SUPERINTENDENT OF SCHOOLS" ~ "KERN COUNTY OFFICE OF EDUCATION",
           NCES_name == "KEYES UNION SCHOOL" ~ "KEYES UNION",
           NCES_name == "KINGS CANYON UNIFIED" ~ "KINGS CANYON JOINT UNF",
           NCES_name == "LAKESIDE UNION ELEMENTARY" ~ "LAKESIDE UNION",
           NCES_name == "LANCASTER SCHOOL DISTRICT" ~ "LANCASTER ELEMENTARY",
           NCES_name == "LEMON GROVE SCHOOL DISTRICT" ~ "LEMON GROVE",
           NCES_name == "LENNOX SCHOOL DISTRICT" ~ "LENNOX", 
           NCES_name == "LOS ALTOS ELEMENTARY SCHOOL DISTRICT" ~ "LOS ALTOS ELEM",
           NCES_name == "MARIN PUPIL TRANSPORTATION AUTHORITY" ~ "MARIN PUPIL TRANSPORTATION AGENCY JPA",
           NCES_name == "MIDWAY SCHOOL DISTRICT" ~ "MIDWAY ELEM",
           NCES_name == "MOTHER LODE UNION" ~ "MOTHER LODE UNION ELEM",
           NCES_name == "MOUNTAIN VIEW-LOS ALTOS UNION HIGH SCHOOL DISTRICT" ~ "MOUNTAIN VIEW-LOS ALTOS UNION HIGH",
           NCES_name == "NEVADA COUNTY SUPERINTENDENT OF SCHOOLS" ~ "NEVADA COUNTY OFFICE OF EDUCATION",
           NCES_name == "NICASIO SCHOOL DISTRICT" ~ "NICASIO",
           NCES_name == "NORTH COUNTY JOINT UNION" ~ "NORTH COUNTY JOINT UNION ELEM",
           NCES_name == "NORTH COUNTY REGIONAL OCCUPATIONAL PROGRAM JOINT POWERS AGENCY" ~ "NORTH COUNTY REGIONAL OCCUPATIONAL PROGRAM JOINT POWERS AGEN",
           NCES_name == "OAK GROVE SCHOOL DISTRICT" ~ "OAK GROVE ELEM", # this is santa clara county, sonoma county is oak grove union
           NCES_name == "ORANGE CENTER ELEMENTARY" ~ "ORANGE CENTER",
           NCES_name == "OXNARD SCHOOL DISTRICT" ~ "OXNARD",
           NCES_name == "PACIFICA SCHOOL DISTRICT" ~ "PACIFICA",
           NCES_name == "PETALUMA CITY SCHOOLS" ~ "PETALUMA CITY ELEMENTARY/JOINT UNION",
           NCES_name == "PINER-OLIVET UNION" ~ "PINER-OLIVET UNION ELEM",
           NCES_name == "PLACERVILLE UNION ELEMENTARY SCHOOL DISTRICT"  ~ "PLACERVILLE UNION ELEM",
           NCES_name == "PLEASANT VALLEY SCHOOL DISTRICT" ~ "PLEASANT VALLEY", # Ventura county
           NCES_name == "POINT ARENA SCHOOLS DISTRICT" ~ "POINT ARENA JOINT UNION HIGH",
           NCES_name == "POLLOCK PINES SCHOOL DISTRICT" ~ "POLLOCK PINES ELEM",
           NCES_name == "REDDING SCHOOL DISTRICT" ~ "REDDING ELEM",
           NCES_name == "RESCUE UNION" ~ "RESCUE UNION ELEM",
           NCES_name == "ROSELAND SCHOOL DISTRICT" ~ "ROSELAND",
           NCES_name == "SAN BERNARDINO COUNTY SUPERINTENDENT OF SCHOOLS" ~ "SAN BERNARDINO COUNTY OFFICE OF EDUCATION",
           NCES_name == "SANTA CRUZ CITY SCHOOLS" ~ "SANTA CRUZ CITY ELEM/HIGH",
           NCES_name == "SANTA ROSA CITY SCHOOLS" ~ "SANTA ROSA ELEM-HIGH, CITY OF",
           NCES_name == "SANTEE SCHOOL DISTRICT" ~ "SANTEE ELEM",
           NCES_name == "SIERRA PLUMAS JOINT UNIFIED" ~ "SIERRA-PLUMAS JOINT UNF",
           NCES_name == "SILVER FORK SCHOOL DISTRICT" ~ "SILVER FORK ELEM",
           NCES_name == "SOLVANG SCHOOL DISTRICT" ~ "SOLVANG ELEM",
           NCES_name == "SOUTH COAST ROP" ~ "SOUTH COAST REGIONAL OCCUPATIONAL PROGRAM",
           NCES_name == "SOUTH FORK UNION ELEMENTARY" ~ "SOUTH FORK UNION",
           NCES_name == "SUNNYVALE SCHOOL DISTRICT" ~ "SUNNYVALE",
           NCES_name == "TWAIN HARTE SCHOOL DISTRICT" ~ "TWAIN HARTE",
           NCES_name == "WESTMINSTER SCHOOL DISTRICT" ~ "WESTMINSTER ELEM",
           NCES_name == "WINSHIP-ROBBINS ESD" ~ "WINSHIP-ROBBINS",
           NCES_name == "WINTON SCHOOL DISTRICT" ~ "WINTON",
           TRUE ~ NCES_name      
           )
         ) %>%
  left_join(nces, by = "NCES_name")
sum(!is.na(df4$localid),na.rm=T)/nrow(df4)

unmatched <- df4 %>%
  filter(is.na(localid)) %>%
  select(NCES_name) %>%
  unique()

df5 <- df4 %>% 
  mutate(state = "CA")

write_csv(df5,paste0(paths$payroll,"CA_Payroll.csv"))


