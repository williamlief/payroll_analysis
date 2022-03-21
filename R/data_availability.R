library(tidyverse)

raw <- readRDS("data-raw/clean_payroll_records.RDS")

table <- raw %>% 
  filter(!(state == "ND" & year > 2017)) %>% 
  mutate(teacher = as.logical(teacher)) %>% 
  group_by(state) %>% 
  summarize(yMin = min(year, na.rm=T),
            yMax = max(year, na.rm=T),
            nTotal = n(),
            nTeach = sum(teacher, na.rm=T)) %>% 
  mutate(Years = paste(yMin,yMax,sep="-"), 
         pTeach = nTeach / nTotal,
         pTeach = scales::percent(pTeach, accuracy = 1)) %>% 
  filter(!is.na(state)) %>% 
  select(State = state,
         Years, 
         `Total Records` = nTotal, 
         `Teacher Records` = nTeach,
         `Proportion Teacher` = pTeach)

knitr::kable(table,
      format = "latex",  booktabs = TRUE,
      format.args = list(big.mark = ","),
      align = "l",
      caption = "Payroll Records by State", 
      label = 'datavail')