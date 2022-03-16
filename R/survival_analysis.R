library(tidyverse)
options(collapse_mask = "manip")
library(collapse)

library(survival)
library(survminer)

raw <- readRDS("data/linked_data.rds")

dat <- raw
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

# Coxph -------------------------------------------------------------------


cox <- coxph(Surv(length, status) ~ NCES_leaid, data = dat3)
summary(cox)



### Attrition
# attrition isnt 100% in 2018 and enter 100% in 2002 because duplicate names in a year
n_year.state <- df %>% 
  group_by(year, state, NCES_leaid) %>% 
  count() %>% 
  group_by(year, state) %>% 
  count()

year.state.district.ee <- df %>% 
  group_by(year, state, NCES_leaid) %>% 
  summarize(attrit = mean(last), enter = mean(first), n = n()) %>% 
  filter(state %in% c("GA", "NV", "NY", "PA"))

ggplot(data = year.state.district.ee  %>% 
         filter(
           year==2015 &
             attrit < .4 &
             !is.na(state)) %>% 
         left_join(n_year.state %>% rename("n_dist" = nn)) %>% 
         mutate(label = paste0(state,": n = ", n_dist)),
       aes(x = attrit, y = label)) +
  geom_density_ridges(rel_min_height = 0.001, scale = 1) +
  scale_x_continuous(limits=c(0, .4), expand = c(0.01, 0)) +
  theme_ridges() +
  ylab("State") + xlab("Attrition Rate") +
  ggtitle("Distribution of Attrition Rates by District in 2015")

ggplot(data = year.state.district.ee  %>% 
         filter(
           year %in% c(2012,2013,2014,2015) &
             attrit < .4 &
             !is.na(state)
         ),
       aes(x = attrit, y = state, fill=as.factor(year), alpha = .1)) +
  geom_density_ridges(rel_min_height = 0.001, scale = 1) +
  scale_x_continuous(limits=c(0, .4), expand = c(0.01, 0)) +
  theme_ridges() + 
  ylab("State") + xlab("Attrition Rate") +
  ggtitle("Distribution of Attrition Rates by District in 2012-2015")