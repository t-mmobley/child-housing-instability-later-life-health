# Purpose: Post-MI cleaning
# Author: Taylor M. Mobley
# Date: Oct 27, 2025

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)
library(gtsummary)

# Import ---------------------------------------------------
dat <- readRDS(file=here("Aim_2", "data", "processed", "hrs_fnlim_imputed.rds"))


nontime_vars <- c("hhidpn", "female", "raceth", "sbirth", "rabcohort", "rabyear",
                  "feduc_lt8", "meduc_lt8", "child_forced_move", 
                  "raeduc",  "family_financ", "death_flag", "eos",
                  "father_unemp", "first_father_military", 
                  "lh4e", "high_childhood_moves_v2", "ravetrn", 
                  "raddate", "rabdate", "radyear", "radage", "iwend", "iwbeg",
                  "lastaliveyear", "lastalivemonth", "radage_y", "radage",
                  "knowndeceasedsource")

time_vars <- c("hrswave", "agewave", "mstat", "itot", "hhres", "shlt",  "cesd",
               "adl6a", "iadl5a", "heart", "diab", "hibp", "strok")

# checking coding
tbl_summary(dat %>% select(any_of(nontime_vars), any_of(time_vars)) %>%
              select(-c(hhidpn, iwbeg, iwend, rabdate, raddate)),
            digits = list(everything() ~ c(0)))

# Variable cleaning -----

## TV covariates -----

dat_clean <- dat %>%
  mutate(raceth4 = factor(ifelse(raceth %in% c(3, 5), 3, raceth))) %>%
  mutate(raeduc3 = case_when(raeduc == 1 ~ "Less than high school",
                             raeduc %in% c(2,3) ~ "HS diploma/GED",
                             raeduc %in% c(4,5) ~ "College and above")) %>%
  # recreate 4-level forced move + mobility variable
  mutate(child_move_insec4 = factor(case_when(child_forced_move==1 & high_childhood_moves_v2==1 ~ 4,
                                       child_forced_move==1 & high_childhood_moves_v2==0 ~ 3,
                                       child_forced_move==0 & high_childhood_moves_v2==1 ~ 2,
                                       child_forced_move==0 & high_childhood_moves_v2==0 ~ 1))) %>%
  mutate(married_partnered = case_when(mstat %in% c(1,2,3) ~ 1,
                                       mstat %in% c(4,5,6,7,8) ~ 0)) %>%
  mutate(hhincome_pp = (itot/1000)/sqrt(hhres)) %>%
  mutate(fairpoor_shlt = case_when(shlt %in% c(4,5) ~ 1,
                                   shlt %in% c(1,2,3) ~ 0)) %>%
  mutate(cesd_high = case_when(cesd %in% c("4","5","6","7","8") ~ "1",
                               cesd %in% c("0","1","2", "3") ~ "0",
                               cesd %in% c("NA") ~ "NA")) 
  # Already did this
  # mutate(across(c("heart", "diab", "hibp", "strok"), ~ 
  #                 case_when(.x %in% c(1,2,3) ~ 1,
  #                           .x %in% c(0,4,5,6) ~ 0)))
  
tbl_summary(dat_clean %>% select(c(married_partnered, hhincome_pp, raeduc3,
                                    fairpoor_shlt, cesd, cesd_high, heart, diab,
                                    hibp, strok, raceth, raceth4)), 
            digits = list(everything() ~ c(0))) 

## Functional limitation measures -----
dat_fnlim_long <- dat_clean %>%
  # Create clean functional lim score vars
  mutate(fnlim = adl6a + iadl5a,
         fnlim_bin = ifelse(fnlim >= 1, 1, ifelse(fnlim==0,0,NA))) %>%
  group_by(hhidpn) %>%
    # Fxnal limitations
    mutate(response_fnlim = row_number(),
           first_fnlim_year = min(hrswave),
           fnlim_count = max(response_fnlim),
           baseage_fnlim = ifelse(response_fnlim==1, agewave, NA),
           baseline_fnlim = ifelse(response_fnlim==1, fnlim, NA),
           baseline_fnlim_bin = ifelse(response_fnlim==1, fnlim_bin, NA)) %>%
    # re-define eos
    mutate(eos = ifelse(death_flag==1, "death", 
                        ifelse(max(hrswave)==2022, "censored", 
                               "lost to follow-up"))) %>%
    fill(baseline_fnlim, .direction = "down") %>%
    fill(baseline_fnlim_bin, .direction = "down") %>%
    fill(baseage_fnlim, .direction = "down") %>%
  ungroup()
  
## Childhood finanical insecurity -----

dat_fnlim_long <- dat_fnlim_long %>%
  mutate(poor_family_financ = ifelse(family_financ == "Poor", 1, 0)) %>%
  mutate(poorvar_family_financ = ifelse(family_financ %in% c("Poor","Varied"), 1, 0))

table(dat_fnlim_long$family_financ, dat_fnlim_long$poor_family_financ,
      exclude = NULL)
table(dat_fnlim_long$family_financ, dat_fnlim_long$poorvar_family_financ,
      exclude = NULL)

# Survival variables -----

## 8 observations where age appears after or equal to death.
## I will re-calculate age with the beginning of the interview in these cases
check <- dat_fnlim_long %>% filter(radage <= agewave)
table(check$hrswave, check$radyear, exclude=NULL)

dat_fnlim_long_surv <- dat_fnlim_long %>%
  group_by(hhidpn) %>% 
  # re-calculate by beginning of interview instead of end
  mutate(agewave = ifelse(!is.na(radage) & radage <= agewave, 
                          as.numeric(difftime(iwbeg, rabdate, units=c("days")))/365.25,
                          agewave)) %>%
  # HRS wave variables
  mutate(hrswave_bl = min(hrswave),
         hrswave_last = max(hrswave)) %>%
  mutate(wavediff = lead(hrswave) - hrswave) %>%
  mutate(wavediff = ifelse(is.na(wavediff), 0, wavediff)) %>%
  # Identify study gaps
  mutate(gap = ifelse(wavediff > 2, 1, NA),
         gap_fill = gap) %>%
  fill(gap_fill, .direction = c("down")) %>%
  mutate(gap = ifelse(is.na(gap), 0, gap),
         gap_fill = ifelse(is.na(gap_fill), 0, gap_fill),
         gap_ever = max(gap)) %>%
  mutate(obs = n()) %>%
  # Baseline age
  mutate(age_bl = ifelse(hrswave==min(hrswave), agewave, NA)) %>%
  mutate(age_bl2 = ifelse(agewave==min(agewave), agewave, NA)) %>%
  fill(age_bl, age_bl2, .direction=c("downup")) %>%
  # time on study
  mutate(time_start = agewave - age_bl) %>%
  ungroup() %>%
  select(c(hhidpn, agewave, time_start, hrswave,
           hrswave_bl, hrswave_last, everything()))

cor(dat_fnlim_long_surv$age_bl, dat_fnlim_long_surv$age_bl2)
length(unique(dat_fnlim_long$hhidpn)) # 37986

# 0 obs!
check <- dat_fnlim_long_surv %>% filter(radage <= agewave)

# Long interval dataset ------

dat_fnlim_wgts <- dat_fnlim_long_surv %>%
  group_by(hhidpn) %>%
  mutate(agewave_end = lead(agewave),
         hrswave_end = lead(hrswave)) %>%
  mutate(ltfu = 0,
         death = 0) %>%
  ## Add death ages and years to age_end and hrswave_end vars
  mutate(agewave_end = ifelse(is.na(agewave_end) & !is.na(radage), 
                              radage, agewave_end)) %>%
  mutate(hrswave_end = ifelse(is.na(hrswave_end) & !is.na(radage), 
                              radyear, hrswave_end)) %>%
  ## Add ltfu ages and years to age_end and hrswave_end vars
  mutate(agewave_end = ifelse(is.na(agewave_end) & hrswave==max(hrswave) & max(hrswave) !=2022,
                              agewave + 2, agewave_end)) %>%
  mutate(hrswave_end = ifelse(is.na(hrswave_end) & hrswave==max(hrswave) & max(hrswave) !=2022,
                              hrswave + 2, hrswave_end)) %>%
  ## Time-varying death indicator
  mutate(death = ifelse(is.na(radage), death,
                        ifelse(agewave_end == radage, 1, death))) %>%
  ungroup() %>%
  select(c(hhidpn, agewave, agewave_end, hrswave, hrswave_end, time_start,
           ltfu, death, radyear, everything()))

# checks
dat_fnlim_wgts %>% filter(agewave_end <= agewave) %>% nrow()

## Note: 2491 people die the same year of their last HRS wave
check <- dat_fnlim_wgts %>% filter(hrswave_end <= hrswave)
cor(check$radyear, check$hrswave)

## No hrswave_end < hrswave
dat_fnlim_wgts %>% filter(hrswave_end < hrswave) %>% nrow()

## Remove rows missing agewave_end (everyone's last row) -----

dat_fnlim_wgts_clean <- dat_fnlim_wgts %>%
  filter(!is.na(agewave_end)) %>%
  group_by(hhidpn) %>%
  ## Time-varying ltfu indicator
  mutate(ltfu = ifelse(hrswave_last!=2022 & 
                         hrswave_end==max(hrswave_end, na.rm=TRUE) & is.na(radage), 
                       1, ltfu)) %>%
  ## Time-varying death or ltfu indicator
  mutate(death_ltfu = ifelse(death==1 | ltfu == 1, 1, 0)) %>%
  mutate(death = ifelse(ltfu == 1, NA, death)) %>%
  ## Add time_end variable
  mutate(time_end = agewave_end - age_bl) %>%
  ungroup() %>%
  select(c(hhidpn, agewave, agewave_end, hrswave, hrswave_end, time_start,
           time_end, ltfu, death, radyear, everything()))

length(unique(dat_fnlim_wgts_clean$hhidpn)) ## 35367

dat_fnlim_wgts_clean %>% filter(time_end <= time_start) %>% nrow()

table(dat_fnlim_wgts_clean$ltfu, dat_fnlim_wgts_clean$death, exclude=NULL)

# Check people who aren't in the interval dataset

## All have 1 fnlim_count in 2022 - makes sense!
check <- dat_fnlim_long_surv %>% filter(!(hhidpn %in% dat_fnlim_wgts_clean$hhidpn))
table(check$fnlim_count, exclude = NULL)
table(check$hrswave, exclude = NULL)

# Gaps 
# per convo with Juliet 2/19/2026, we think the weights are correctly etimated
# with gaps because people with a gap won't contribute to the waves where they
# are missing data and probs will be correctly specified
# check gaps
table(dat_fnlim_long_surv$gap, dat_fnlim_long_surv$gap_ever, exclude=NULL)
table(dat_fnlim_long_surv$gap_fill, dat_fnlim_long_surv$gap_ever, exclude=NULL)

t <- dat_fnlim_long_surv %>% filter(gap_ever==1)
length(unique(t$hhidpn)) # 4850 out of 37986 (12.8%) ever have a gap

# Save -----

saveRDS(dat_fnlim_long_surv, here("Aim_2", "data", "processed", "dat_fnlim_long.rds"))
saveRDS(dat_fnlim_wgts_clean, here("Aim_2", "data", "processed", "dat_fnlim_interval.rds"))
