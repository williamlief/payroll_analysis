library(tidyverse)

OK_det <- readRDS("data/deterministic/OK.RDS")
OK_fl <- readRDS("data/fastLink/OK.RDS")

OK <- OK_det %>% 
  tidylog::left_join(OK_fl %>% select(source_id, year, flid)) %>% 
  dplyr::select(-starts_with("detid_")) %>% 
  mutate(source_id = stringr::str_pad(source_id, 6, pad = "0", side = "left"))

# Accuracy ----------------------------------



f.measure.by.year <- function(dat, source.idvar, constructed.idvar, yearvar) {
  
  dat$.sourceid <- dat[[source.idvar]]
  dat$.constructid <- dat[[constructed.idvar]]
  dat$.year <- dat[[yearvar]]
  
  years <- sort(unique(dat$.year))
  
  ## F Measure 
  # True match 
  d <- vector(mode = "numeric", length = length(years)-1)
  for (i in 2:length(years)) {
    
    d[[i-1]] <- dat %>% 
      filter(.year %in% !!years[c(i-1, i)]) %>% 
      group_by(.sourceid) %>% 
      filter(n() == 2) %>% 
      group_by(.constructid) %>% 
      filter(n() == 2) %>% 
      ungroup() %>% 
      nrow()
    
  }
  
  # false non-match
  c  <- vector(mode = "numeric", length = length(years)-1)
  for (i in 2:length(years)) {
    
    c[[i-1]] <- dat %>%
      filter(.year %in% !!years[c(i-1, i)]) %>% 
      group_by(.sourceid) %>% 
      filter(n() == 2) %>% 
      group_by(.constructid) %>% 
      filter(n() != 2) %>% 
      ungroup() %>% 
      nrow()
  }
  
  
  # False match 
  b  <- vector(mode = "numeric", length = length(years)-1)
  for (i in 2:length(years)) {
    
    b[[i-1]] <- dat %>% 
      filter(.year %in% !!years[c(i-1, i)]) %>% 
      group_by(.sourceid) %>% 
      filter(n() == 1) %>% 
      group_by(.constructid) %>% 
      filter(n() == 2) %>% 
      ungroup() %>% 
      nrow()
  }
  
  # true non-match 
  a <- vector(mode = "numeric", length = length(years)-1)
  for (i in 2:length(years)) {
    
    a[[i-1]] <- dat %>% 
      filter(.year %in% !!years[c(i-1, i)]) %>% 
      group_by(.sourceid) %>% 
      filter(n() == 1) %>% 
      group_by(.constructid) %>% 
      filter(n() == 1) %>% 
      ungroup() %>% 
      nrow()
  } 
  return(list('true.match' = d,
              'false.non.match' = c, 
              'false.match' = b, 
              'true.non.match' = a
              ))
}

f_detid <- f.measure.by.year(OK, "source_id", "detid", "year")
f_fastLink <- f.measure.by.year(OK, "source_id", "flid", "year")


f.measure.calc <- function(f_list) {
  2*f_list$true.match / 
    (f_list$false.non.match + f_list$false.match + 2*f_list$true.match)
} 


f.measure.calc(f_detid)
f.measure.calc(f_fastLink)

f.measure.agg <- function(f_list) {
  list(
    true.match      = sum(f_list$true.match),
    false.non.match = sum(f_list$false.non.match),
    false.match     = sum(f_list$false.match),
    true.non.match  = sum(f_list$true.non.match)
  )
}

precision <- function(f_list) {
  f_list$true.match / (f_list$true.match + f_list$false.match)
}

recall <- function(f_list) {
  f_list$true.match / (f_list$true.match + f_list$false.non.match)
}

f.measure.agg.df <- function(f_list) {
  
 f2 <- f.measure.agg(f_list)
  
  data.frame(`True Matches` = f2$true.match,
             `False Non-Match` = f2$false.non.match, 
             `False Match` = f2$false.match, 
             `True Non-Match` = f2$true.non.match,
             `Precision` = precision(f2),
             `Recall` = recall(f2),
             `F Measure` = f.measure.calc(f2),
             check.names = FALSE)
}


accuracy_table <- bind_rows(
 "Deterministic Link" = f.measure.agg.df(f_detid),
 "Probabilistic Link" = f.measure.agg.df(f_fastLink),
 .id = "Method"
)


knitr::kable(accuracy_table %>% pivot_longer(-Method) %>% 
               pivot_wider(names_from = Method, values_from = value), 
             format = 'latex', booktabs = TRUE,
             # vline = "", 
             # toprule = "\\hline \\hline", 
             # midrule = "",
             # bottomrule = "",
             format.arg = list(big.mark = ","),
             digits = 3,
             caption = 'Accuracy of deterministic linkage, evaluating each year-pair in Oklahoma', 
             label = 'linkacc')


false.match.percent.matches <- function(f_list) {
  f_list$false.match / (f_list$false.match + f_list$true.match)
}


false.non.match.percent.non.matches <- function(f_list) {
  f_list$false.non.match / (f_list$false.non.match + f_list$true.non.match)
}

# detid
false.match.percent.matches(f.measure.agg(f_detid))
false.non.match.percent.non.matches(f.measure.agg(f_detid))

# fastlink
false.match.percent.matches(f.measure.agg(f_fastLink))
false.non.match.percent.non.matches(f.measure.agg(f_fastLink))

# Turnover Comparison -----------------------

turn_pipe <- function(dat, id, ...) {
  micro <- dat %>% 
    select(teacher, position, year, {{id}}, ...) %>% 
    group_by(..., {{id}}) %>% 
    arrange({{id}}, year) %>% 
    mutate(turnover = is.na(lead(year)) | lead(year) != year + 1,
           year = year + 1) %>% 
    ungroup() 
  
  
  micro %>%
    filter(teacher == "TRUE") %>% 
    group_by(year) %>% 
    summarize(
      turnover = mean(turnover, na.rm = TRUE),
      .groups = "drop"
    )
}

turns <- list()

# teacher-state
turns[["Official ID"]] <- OK %>% 
  filter(teacher == "TRUE") %>% 
  turn_pipe(id = source_id)

turns[["Deterministic"]] <- OK %>% 
  filter(teacher == "TRUE") %>% 
  turn_pipe(id = detid)

turns[["Probabilistic"]] <- OK %>% 
  filter(teacher == "TRUE") %>% 
  turn_pipe(id = flid)

turn_dat <- bind_rows(turns, .id = "turnover_type")

ggplot(data = turn_dat %>% filter(year != max(year)) %>% 
         mutate(year = paste0(year-1, "-", year),
                label = if_else(year == max(year), turnover_type, NA_character_)), 
       aes(x = factor(year), y = turnover, 
           color = turnover_type, group = turnover_type)) +
  geom_line() +
  labs(title = "Overall Teacher Turnover Estimates by Linkage Method", 
       x = "", 
       y = "Teacher Turnover Rate") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(clip = 'off') +
  geom_text(aes(label = label), hjust = 0, nudge_x = 0.1) +
  theme(legend.position = 'none',
        plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm")) 

ggsave("figures/linkCompare.png", 
       bg = "white", width = 7, height = 4)