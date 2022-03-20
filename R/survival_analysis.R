library(tidyverse)
options(collapse_mask = "manip")
library(collapse)

library(survival)
library(survminer)

library(ggridges)

raw <- readRDS("data/linked_data.rds")

dat <- raw %>% 
  filter(!(state == "ND" & year > 2017)) # ND has a hole that messes up turnover rates
# dat <- sample_frac(raw, .001)


# Turnover plot -----------------------------------------------------------

turn_pipe <- function(dat, ...) {
  micro <- dat %>% 
    select(teacher, year, id, state, ...) %>% 
    group_by(..., state, id) %>% 
    arrange(state, id, year) %>% 
    mutate(turnover = is.na(lead(year)) | lead(year) != year + 1,
           year = year + 1) %>% 
    ungroup() 
  
  
  micro %>%
    filter(teacher == "TRUE") %>% 
    group_by(year, state) %>% 
    summarize(
      turnover = mean(turnover, na.rm = TRUE),
      .groups = "drop"
    )
}

turns <- list()

# teacher-state
turns[["Teacher State"]] <- dat %>% 
  filter(teacher == "TRUE") %>% 
  turn_pipe()

# State
turns[["Any Job State"]] <- dat %>% 
  turn_pipe()

# teacher-district !! standard
turns[["Teacher District"]] <- dat %>% 
  filter(teacher == "TRUE") %>%
  turn_pipe(NCES_leaid)

# district
turns[["Any Job District"]] <- dat %>% 
  turn_pipe(NCES_leaid)

turn_dat <- bind_rows(turns, .id = "turnover_type") 

turn_dat2 <- turn_dat %>% 
  # filter(year >= 2000) %>% 
  group_by(state) %>% 
  filter(year != max(year)) %>% 
  mutate(state_fmt = paste0(state, " (", min(year), "-", max(year),")")) %>% 
  ungroup() 
  

p <- ggplot(data = turn_dat2, 
       aes(x = year, y = turnover, 
           color = turnover_type, group = turnover_type)) +
  geom_line() +
  facet_wrap(~state_fmt, scales = "free") +
  labs(title = "Teacher Turnover: District and State", 
       x = "", 
       y = "Teacher Turnover Rate", 
       color = NULL,
       caption = str_wrap("Records linked with staff ids in OK and NE, all other states linked using methods described in this paper's record linkage section.", 120)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        legend.position = "bottom") +
  scale_y_continuous(labels = function(x) {scales::percent(x, accuracy = 1)}) 
  # coord_cartesian(clip = 'off') +
  # geom_text(aes(label = label), hjust = 0, nudge_x = 0.1) +
  # theme(legend.position = 'none',
  #       plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) 

ggsave("figures/turnover_comparison.png",
       plot = p,
       bg = "white", width = 7, height = 7)


## District level turnover ------------------------------------

# District level turnover - this takes a while to run
micro_dist_turn <- dat %>% 
  filter(teacher == "TRUE") %>% 
  select(teacher, NCES_leaid, year, id, state) %>% 
  group_by(state, NCES_leaid, id) %>% 
  arrange(state, id, year) %>% 
  mutate(turnover = is.na(lead(year)) | lead(year) != year + 1,
         year = year + 1) %>% 
  ungroup() 

dist_turnover <- micro_dist_turn %>% 
  group_by(NCES_leaid, state) %>% 
  filter(year != max(year)) %>% # Last year has 100% turnover!
  group_by(NCES_leaid, state, year) %>% 
  summarize(
    turnover = mean(turnover, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) 

dist_turnover  %>% 
  filter(year %in% c(2011:2017), n >= 100) %>% 
  mutate(exclude = turnover > .4) %>% 
  count(exclude) %>% 
  mutate(p = n / sum(n)) %>% 
  filter(exclude == TRUE) %>%
  pull(p) %>% 
  scales::percent(., accuracy = .1) -> p_excl
           

p <- ggplot(data = dist_turnover  %>% 
         filter(year %in% c(2011:2017), n >= 100, turnover < .4),
       aes(x = turnover, y = fct_rev(factor(year)),  alpha = .1)) +
  geom_density_ridges(rel_min_height = 0.001, scale = 1) +
  facet_wrap(.~state) +
  coord_cartesian(xlim = c(0, .4)) +
  scale_alpha(guide = 'none') +
  theme_ridges() + 
  labs(y = NULL, alpha = NULL,
       x = "Turnover Rate",
       title = "Distribution of Turnover Rates by District in 2011-2017",
       caption = str_wrap(paste("Figure restricted to districts with 100 or more teachers. For clarity, figure restricts x axis to 40% maximum annual turnover, excluding", p_excl, "of observations")), 150)

ggsave(plot = p, 
       filename = "figures/turnover_ridge.png",
       width = 7, 
       height = 7)

dist_turn_table <- dist_turnover %>% 
  filter(n >= 100) %>% 
  group_by(state) %>% 
  summarize(
    p25 = quantile(turnover, .25),
    median = quantile(turnover, .5), 
    p75 = quantile(turnover, .75),
    mean = mean(turnover), 
    sd = sd(turnover), 
    n = n(), 
    years = paste0(min(year), ":", max(year)))

knitr::kable(dist_turn_table, booktabs = TRUE,
             format = "latex", 
             digits = 2,
             caption = "Turnover rates for teaching positions within a district", 
             label = "distturn")


# Survival Plots -----------------------------------------------------------

state_stats <- dat %>% 
  group_by(state) %>% 
  summarize(state_start = min(year),
            state_end =   max(year))




## Teachers -------------------------------------------------------------

dat2 <- dat %>% 
  group_by(state, id) %>% 
  filter(teacher == "TRUE") %>% 
  summarize(year_first = min(year), 
            year_last = max(year), 
            n_years = n()) %>% 
  left_join(state_stats)


dat3 <- dat2 %>% 
  mutate(length = year_last-year_first + 1,
         length = n_years,
         status = case_when(year_last < state_end ~ 1,
                            TRUE ~ 0)) %>% 
  filter(year_first != state_start) %>% # exclude first year b.c. left trunc
  mutate(state_fmt = paste0(state, " (", state_start+1, "-", state_end,")"))

fit <- survfit(Surv(length, status) ~ state_fmt, data = dat3)

# Median Table 

# https://www.bls.gov/news.release/pdf/tenure.pdf
bls_stats <- data.frame(
  Profession = c("Management occupations", "Business and financial operations occupations",
           "Computer and mathematical occupations", "Architecture and engineering occupations",
           "Life, physical, and social science occupations", "Community and social service occupations",
           "Legal occupations", "Education, training, and library occupations",
           "Arts, design, entertainment, sports, and media occupations", "Healthcare practitioners and technical occupations"
           ),
  `Median Employment` = c(5.8, 4.7, 3.9, 5.1, 4.1, 4.6, 5.8, 5.0, 3.4, 4.7),
  check.names = FALSE
) 

med_surv <- surv_median(fit) %>% 
  mutate(state = gsub("state_fmt=", "", strata)) %>% 
  select(State = state, `Median Employment` = median)


knitr::kable(med_surv, booktabs = TRUE,
             format = "latex", 
             caption = "Median employment length as a teacher in a given district by state contrasted with BLS statistics on median employment by profession. States with missing values did not have enough years of records to observe the number of years taken for half of teachers to exit", 
             label = "medianEmp")

knitr::kable(bls_stats, booktabs = TRUE,
             format = "latex")

# summary(fit)
ggsurv <- ggsurvplot(fit, data = dat3, risk.table = TRUE,  break.time.by = 1) 

p <- ggsurv$plot + 
  facet_wrap(~state_fmt) +
  theme_bw() + 
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 10)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(title = "Survival Analysis - Teachers", 
       y = "Percent of Staff Retained", 
       x = "Years Employed As Teacher")

ggsave(plot = p, 
       filename = "figures/state_survival_analysis_teacher.png",
       width = 7, 
       height = 7)



rm(dat2, dat3, fit, ggsurv, p)

## Any position ------------------------------------------------------------

dat2 <- dat %>% 
  group_by(state, id) %>% 
  filter(teacher == "TRUE" | year > min(year)) %>% # keep only people who started as teachers
  summarize(year_first = min(year), 
            year_last = max(year)) %>% 
  left_join(state_stats)


dat3 <- dat2 %>% 
  mutate(length = year_last-year_first + 1,
         status = case_when(year_last < state_end ~ 1,
                            TRUE ~ 0)) %>% 
  filter(year_first != state_start) %>% # exclude first year b.c. left trunc
  mutate(state_fmt = paste0(state, " (", state_start+1, "-", state_end,")"))

fit <- survfit(Surv(length, status) ~ state_fmt, data = dat3)

# summary(fit)
ggsurv <- ggsurvplot(fit, data = dat3, risk.table = TRUE,  break.time.by = 1) 

p <- ggsurv$plot + 
  facet_wrap(~state_fmt) +
  theme_bw() + 
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0, 10)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(title = "Survival Analysis - Any Position", 
       y = "Percent of Staff Retained", 
       x = "Years Employed in Public Education in State")

ggsave(plot = p, 
       filename = "figures/state_survival_analysis_anyjob.png",
       width = 7, 
       height = 7)

rm(dat2, dat3, fit, ggsurv, p)


# Median survival time by district  ------------------------------------------

dat2 <- dat %>% 
  filter(teacher == "TRUE") %>% 
  group_by(state, NCES_leaid, id) %>% 
  summarize(year_first = min(year), 
            year_last = max(year), 
            n_years = n()) %>% 
  left_join(state_stats)

dat3 <- dat2 %>% 
  mutate(length = year_last-year_first + 1,
         length = n_years,
         status = case_when(year_last < state_end ~ 1,
                            TRUE ~ 0)) %>% 
  filter(year_first != state_start) # exclude first year b.c. left trunc

fit <- survfit(Surv(length, status) ~ NCES_leaid, data = dat3)

med_surv <- surv_median(fit) %>% 
  mutate(NCES_leaid = gsub("NCES_leaid=", "", strata))


dist_turnover_agg <- micro_dist_turn %>% 
  group_by(NCES_leaid, state) %>% 
  filter(year != max(year)) %>% # Last year has 100% turnover!
  group_by(NCES_leaid, state) %>% # no year, overall average
  summarize(
    turnover = mean(turnover, na.rm = TRUE),
    n = n() / (max(year) - min(year) + 1),
    .groups = "drop"
  ) 

# Combine 
dat4 <- 
  med_surv %>% 
  tidylog::inner_join(dist_turnover_agg, by = "NCES_leaid")


p <- ggplot(data = dat4, aes(x = turnover, y = median, size = n)) + 
  geom_point(alpha = 0.5) + 
  theme_minimal() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(y = "Median Employment Length", x = "District Average Annual Turnover Rate",
       title = "Length of Employment and Turnover", 
       subtitle = "Each point represents a district", 
       size = "Number of Teachers")

ggsave(plot = p, 
       filename = "figures/median_turnover.png",
       width = 7, 
       height = 7)

