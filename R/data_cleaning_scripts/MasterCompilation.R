source('libs_and_paths.R', echo=TRUE)
library(vroom)

files <- list.files(path = paths$payroll, 
                    pattern = ".Payroll.csv",
                    full.names = TRUE)

raw <- lapply(files, vroom, 
             col_types = cols_only(
               name_first = "c", name_middle = "c", name_last = "c", name_other = "c",
               source_id = "c",
               position = "c", teacher = "c",  fte = 'c',
               salary = "n", salary_total = "n", salary_base = "n", salary_fringe = "n", salary_other = "n", salary_extra_duty = "n", 
               NCES_name = "c", NCES_type = "c", NCES_leaid = "c", localid = "c", 
               year = "n", state = "c", source = "c"))
names(raw) <- str_remove(str_extract(files, "Data/.*_"), "Data/")

# THis should be part of data cleaning
raw[["CA_"]] <- raw[["CA_"]] %>%  
  mutate(salary = salary_total)

raw[["NV_"]] <- raw[["NV_"]] %>%  
  mutate(salary = salary_total) 
raw[["OK_"]] <- raw[["OK_"]] %>%  
  mutate(salary = salary_base + salary_fringe + salary_other + salary_extra_duty) 



df <- bind_rows(raw, .id = "filename")
df <- df %>% select(-c(starts_with("salary_")))

table(df$state)
table(df$year, df$state)

# percent teacher by state
df %>% group_by(state) %>% summarize(pct_teach = mean(teacher == "TRUE", na.rm = T))
# 158,517 position titles
pos <- df %>% count(position = tolower(position), teacher, sort = T)
write_csv(pos, "position_map_raw.csv")
clean_pos <- read_csv("position_map_manual.csv", col_types = cols(.default = "c")) %>% 
  select(1:4) 

df2 <- df %>%
  mutate(position = tolower(position)) %>% 
  tidylog::left_join(clean_pos %>% mutate(teacher = as.character(teacher))) %>% 
  mutate(teacher = coalesce(as.character(teacher_new), teacher), 
         teacher = if_else(is.na(teacher), "FALSE", teacher))

df2 %>% group_by(state) %>% summarize(pct_teach = mean(teacher == "TRUE", na.rm = T))

# percent NA by variable and state
p_na <- df2 %>% 
  group_by(state) %>% 
  summarize_all(list(~mean(is.na(.)))) 


# Percent of records matched to NCES districts by state
df2 %>% group_by(state) %>% mutate(hasNCES = !is.na(NCES_leaid)) %>% summarize(pct_matched = mean(hasNCES))

df_match <- df2 %>% tidylog::filter(!is.na(NCES_leaid))

df_match %>% group_by(state) %>% summarize(pct_teach = mean(teacher == "TRUE", na.rm = T))

df_final <- df_match %>% 
  select(-c(teacher_new, NCES_name, localid, NCES_type, source, n))
saveRDS(df_final, "clean_payroll_records.RDS")