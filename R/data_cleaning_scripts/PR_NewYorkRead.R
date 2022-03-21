# NY State Data
setwd(paste0(stubs$dh,"New York/Payroll"))

list <- list.files(pattern="*.xlsx")
read <- lapply(list, read_excel, col_types = "text", na="NDR") %>% bind_rows()

# Names are formatted as 
# last, first middle, other

# position and membership date are almost always missing
sum(is.na(read$PositionName))/nrow(read)
sum(is.na(read$`Membership Date`))/nrow(read)
sum(is.na(read$MembershipDate))/nrow(read)

nys <- read %>% 
  mutate(WholeName = gsub(",,", ",", WholeName),
         WholeName = gsub("Donigian-Ott, Am", "Donigian-Ottam", WholeName),
         WholeName = tolower(WholeName),
         salary=as.numeric(YTDPay),
         year = as.numeric(PayYear), 
         state = "NY",
         source = "Empire Center for Public Policy (non-govt)",
         school_district = tolower(AgencyName),
         temp= WholeName,
         teacher = SubAgencyName == "NYSTRS - Educator") %>% 
  separate(WholeName, into = c("name_last","name_firstmiddle","name_other"), sep=",") %>% 
  mutate_at(c("name_last","name_firstmiddle","name_other"),trimws) %>% 
  mutate(name_firstmiddle = sub(" ",",",name_firstmiddle)) %>% 
  separate(name_firstmiddle, into=c("name_first","name_middle"), sep =",") %>% 
  select(state, year, salary, name_first, name_middle, name_last, name_other, school_district, teacher, source)

###
# NCES Match
dist <- nys %>% 
  group_by(school_district) %>% 
  count()

nces <- read_csv(paste0(paths$agg, "/NCES_CCD.csv")) %>% filter(state=="NY") %>% 
  mutate(NCES_name = tolower(NCES_name)) %>% 
  filter(year != 2017) %>% # nces appends a state prefix to leaid in this year
  select(-year) %>% 
  unique()

nces2 <- nces %>% 
  mutate(NCES_name = str_replace(NCES_name, "city",""),
         NCES_name = str_replace(NCES_name, "high school district","school district"),
         NCES_name = str_replace(NCES_name, "csd","central school district"),
         NCES_name = str_replace( NCES_name, "-", " ")) %>% 
  filter(localid != 260501861002 & 
         NCES_leaid != 3601007) %>% # these are two charters that switch their ids, get rid of the older id 
  unique()

dist2 <- dist %>% 
  mutate(NCES_name = school_district,
         NCES_name = str_replace( NCES_name, "schools", "school district"),
         NCES_name = str_replace( NCES_name, "au sable", "ausable"),
         NCES_name = str_replace( NCES_name, "ford edward", "fort edward"),
         NCES_name = str_replace( NCES_name, "#", ""),
         NCES_name = str_replace( NCES_name, "public", ""),
         NCES_name = str_replace( NCES_name, "city", ""),
         NCES_name = str_replace( NCES_name, "-", " ")) %>% 
    left_join(nces2)
sum(!is.na(dist2$NCES_leaid))/nrow(dist2)

test <- dist2 %>% filter(is.na(NCES_leaid))
write.csv(test %>% select(school_district, sd2 = NCES_name), "NY_nces_match.csv")
# manually edited file to match
link <- read.csv("NY_nces_match_edit.csv") %>% 
  mutate_all(funs(str_replace_all(.,"\xca"," "))) %>% 
  select(-X, -sd2, new_NCES_name = NCES_name)

dist3 <- dist2 %>% 
  left_join(link, by = "school_district") %>% 
  mutate(NCES_name = ifelse(is.na(NCES_leaid) & !is.na(new_NCES_name), new_NCES_name, NCES_name)) %>% 
  select(-c(state, localid, NCES_type, NCES_leaid)) %>% 
  left_join(nces2) %>% 
  select(-new_NCES_name, n)
sum(!is.na(dist3$NCES_leaid))/nrow(dist3) 

nys2 <- nys %>% 
  left_join(dist3)
sum(!is.na(nys2$NCES_leaid))/nrow(nys2) 

table(nys2$year)
# the data is bad for 2009 and 2008, so will get rid of it here
nys3 <- nys2 %>% filter(!(year %in% c(2008,2009)))

write_csv(nys3,paste0(paths$payroll,"NY_Payroll.csv"))

