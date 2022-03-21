source("libs_and_paths.R")

file_specs <- list(
  "2008ECR" = c(sheet = "08Tchr", skip = 5), 
  "2010ECR" = c(sheet = "Teacher Detail", skip = 7),
  "2011ECR" = c(sheet = "Teachers", skip = 1),
  "2013ECR" = c(sheet = "Teachers", skip = 1),
  "2014ECR" = c(sheet = "Teachers", skip = 1),
  "2015ECR" = c(sheet = "Teachers", skip = 1),
  "2016ECR" = c(sheet = "Teachers", skip = 1),
  "2017ECR" = c(sheet = "Teachers", skip = 1),
  "2019ECR" = c(sheet = "Teachers", skip = 1),
  "2020ECR" = c(sheet = "Teachers", skip = 1),
  "2021ECR" = c(sheet = "Teachers", skip = 1))

files <- list.files(path = paste0(stubs$dh,"North Dakota/Payroll"), 
                   pattern = ".xlsx", full.names=TRUE) 
names(files) <- str_extract(files, "\\d{4}ECR")

files_to_read <- files[6:13]
raw <- map_dfr(files_to_read, 
               read_excel, col_types = "text", sheet = "Teachers", skip = 1,
               .id = "filename")

glimpse(raw)
liefTools::summarize_na(raw, filename)

dat <- raw %>% 
  transmute(state = "ND", 
            year = substr(filename, 1, 4), 
            salary = SALincCOMP, 
            name_first = NULL, 
            name_middle = NULL, 
            name_last = NULL, 
            name_other = NULL,
            source_id = LicenseNumber, 
            official_start = NULL,
            teacher = P1_Desc == "Teacher", 
            school_district = tolower(DistrictName), 
            position = P1_Desc, 
            source = "ND Dept. of Ed"
  )

nces <- read_csv(paste0(paths$agg, "NCES_CCD.csv"), col_types = "dcccdc") %>% 
  filter(state=="ND") %>% 
  mutate(NCES_name = tolower(NCES_name), 
         localid = str_remove(localid, "ND-")) %>% 
  select(-c(year, state, NCES_type)) %>% 
  unique() 

dat2 <- dat %>% 
  tidylog::left_join(nces, by = c("school_district" = "NCES_name"), keep = T)

write_csv(dat2, paste0(paths$payroll, "ND_Payroll.csv"))