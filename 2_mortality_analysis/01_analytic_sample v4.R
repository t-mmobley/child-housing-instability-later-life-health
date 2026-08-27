# Purpose: Create analytic sample for mortality
# Author: Taylor M. Mobley
# Date: Nov 3, 2024

library(here)
source(here("Aim_1/code/0a_libraries.R"))

# Import ---------------------------------------------------
dat <- readRDS(here("data", "processed", "hrs_lhms_clean.rds"))

length(unique(dat$hhidpn)) # 38402

# Further restrictions -----

## year enter != 2022 -----
dat <- dat %>%
  group_by(hhidpn) %>%
  filter(min(hrswave) != 2022) %>%
  ungroup()

length(unique(dat$hhidpn)) # now 35405 (was 34,472 (was 34,475 with agey_e restriction))

# survival analysis variables ------------------------------

## Version 1 -----

hrs_lhms_surv <- dat %>%
  group_by(hhidpn) %>%
  mutate(hrswave_first = first(na.omit(hrswave)),
         age_bl = ifelse(hrswave==min(hrswave), agewave, NA),
         age_bl2 = min(agewave)) %>%
  # note: these are the same. See check below
  fill(age_bl, age_bl2, hrswave_first, .direction=c("downup")) %>% 
  mutate(age_start = age_bl) %>%
  mutate(lastiw = max(hrswave)) %>%
  ungroup()

## checks
hrs_lhms_surv %>% filter(age_bl != age_bl2) %>% nrow()

## check death ages -- 3 obs.
## The issue seems to be using iw end (recommended by RAND in longitudinal doc). 
##For these, I will use iw beginning.
check <- hrs_lhms_surv %>% filter(radage < age_start) 

hrs_lhms_surv <- hrs_lhms_surv %>%
  group_by(hhidpn) %>%
  mutate(age_start = ifelse(!is.na(radage) & radage < age_start, 
                            as.numeric(difftime(iwbeg, rabdate, units=c("days")))/365.25,
                            age_start)) %>%
  mutate(age_bl = age_start,
         age_bl2 = age_start) %>%
  ungroup()

## re-check -- 0 obs!
check <- hrs_lhms_surv %>% filter(radage < age_start) 

hrs_lhms_surv <- hrs_lhms_surv %>%
  group_by(hhidpn) %>%
  mutate(age_end = case_when(!is.na(radage) ~ radage,
                             # LTFU at end of the interval
                             hrswave==lastiw & max(hrswave) <= 2020 ~ agewave,
                             # Censored at 2022 wave
                             hrswave==lastiw & max(hrswave) == 2022 ~ agewave),
         test = ifelse(!is.na(radage), radage, max(agewave)),
         status_death = case_when(!is.na(radage) ~ 1,
                                  is.na(radage) ~ 0),
         status_ltfu = ifelse(eos=="loss to follow-up", 1, 0),
         status2 = case_when(eos=="death" ~ 1,
                             eos=="lost to follow-up" ~ 1,
                             eos=="censored" ~ 0),
         status3 = case_when(eos=="death" ~ 1,
                             eos=="lost to follow-up" ~ 2,
                             eos=="censored" ~ 0)) %>%
  fill(age_end, test, .direction=c("updown")) %>%
  ungroup()

# check age_end coding
cor(hrs_lhms_surv$test, hrs_lhms_surv$age_end)
hrs_lhms_surv %>% filter(test != age_end) %>% nrow()

# age at LHMS -- 2 people age lhms < 50. Don't count them for this analysis
summary(hrs_lhms_surv$age_lhms)
t <- hrs_lhms_surv %>% filter(age_lhms < 50)
length(unique(t$hhidpn))

hrs_lhms_surv <- hrs_lhms_surv %>%
  mutate(lhms_flag = ifelse(age_lhms < 50, NA, lhms_flag),
         age_lhms = ifelse(age_lhms < 50, NA, age_lhms))

# re-check
summary(hrs_lhms_surv$age_lhms)
t <- hrs_lhms_surv %>% filter(age_lhms < 50)
length(unique(t$hhidpn))

hrs_lhms_surv <- hrs_lhms_surv %>%
  group_by(hhidpn) %>%
  # if age_end==age_start set to 0.0001 + age_start 
  mutate(age_end = ifelse(age_end == age_start, age_end + 0.0001, age_end)) %>%
  fill(age_end, test, .direction=c("updown")) %>%
  mutate(time = age_end - age_start,
         time_lhms = ifelse(is.na(age_lhms), NA, age_end - age_lhms)) %>%
  ungroup() %>%
  select(c(hhidpn, hrswave, age_bl, agey_e, age_start, age_end, age_lhms, 
           radage, lhms_year, time, time_lhms, eos, hrswave_first, hrswave_last, 
           lastiw, iwend, raddate, iwbeg, iwmid, starts_with("status"), 
           everything()))

# all age_end should be > 50
min(hrs_lhms_surv$age_end) 
summary(hrs_lhms_surv$age_end)

# All time should be > 0
min(hrs_lhms_surv$time) 
summary(hrs_lhms_surv$time)

# LHMS survey - 500 people with time_lhms < 0. See how lastalive date changes this
t <- hrs_lhms_surv %>% filter(!is.na(time_lhms) & time_lhms < 0)
length(unique(t$hhidpn))
summary(hrs_lhms_surv$time_lhms) 
table(t$eos, exclude=NULL)

# Should all be 0
hrs_lhms_surv %>% filter(age_end <= age_start) %>% nrow()

## check death_flag + status variables 
table(hrs_lhms_surv$death_flag, hrs_lhms_surv$eos, exclude = NULL)
table(hrs_lhms_surv$death_flag, hrs_lhms_surv$status3, exclude = NULL)

## Version 2 -----

hrs_lhms_surv2 <- hrs_lhms_surv %>%
    group_by(hhidpn) %>%
    mutate(lastalivedate = make_date(lastaliveyr, lastalivemo, 15),
           # 2022 HRS wave took place between March 2022 - Dec 2023 
           # (https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/1739293357/h22dd.pdf)
           censordate = make_date(2023, 12, 15),
           iwlast = max(iwend),
           iwlast2 = ifelse(hrswave == last(hrswave), iwend, NA),
           iwlastyear = year(iwlast),
           iwlastmonth = month(iwlast),
           extratime = as.numeric(difftime(lastalivedate, iwlast, units = "days"))/365.25,
           # Set up age_end2
           age_end2 = ifelse(!is.na(radage), age_end, age_end + extratime),
           # Only keep age_end2 value if it is greater than age_end
           age_end2 = ifelse(!is.na(radage), age_end,
                             ifelse(age_end2 > age_end, age_end2, age_end)),
           time2 = age_end2 - age_start,
           time_lhms2 = ifelse(is.na(age_lhms), NA, age_end2 - age_lhms)) %>%
    fill(iwlast2, .direction = c("downup")) %>%
    ungroup() %>%
    select(c(hhidpn, hrswave, agewave, agey_e, age_bl, age_start, age_end, 
             age_end2, time, time2, rabdate, raddate, lhms_date, iwend, iwlast, 
             lastalivedate, radage, censordate, extratime, age_lhms, 
             time_lhms, time_lhms2, 
             everything()))

## Make sure iwlast matches last observed
hrs_lhms_surv2 %>% filter(iwlast != iwlast2) %>% nrow() # 0 obs

## Make sure deaths are all captured
t2 <- hrs_lhms_surv2 %>% filter(!is.na(radage))
t2 %>% filter(age_end!=age_end2) %>% nrow() # 0, as expected
t2 %>% filter(time!=time2) %>% nrow() # 0, as expected
table(t2$death_flag, exclude=NULL) # all 1, as expected

summary(hrs_lhms_surv2$extratime)
summary(hrs_lhms_surv2$iwlastyear)

summary(hrs_lhms_surv2$age_start)
summary(hrs_lhms_surv2$age_end)
summary(hrs_lhms_surv2$time)

summary(hrs_lhms_surv2$age_end2)
summary(hrs_lhms_surv2$time2)

t <- hrs_lhms_surv2 %>% filter(age_end2 != age_end)
length(unique(t$hhidpn)) # 5305 people with different end of FU
table(t$eos, t$hrswave_last, exclude=NULL)

# LHMS -- 13302 people who took LHMS, 148 people still with time < 0
summary(hrs_lhms_surv2$time_lhms2)
t <- hrs_lhms_surv2 %>% filter(lhms_flag==1) %>% distinct(hhidpn, .keep_all=TRUE) 
t2 <- t %>% filter(time_lhms2 < 0)
length(unique(t$hhidpn))
table(t2$eos, exclude=NULL) # 19 deaths, 129 LTFU
table(t2$lh61, exclude=NULL)

## I'm going to set people who die first to missing lhms_flag (prioritizing death age estimate)
## and set others to time_lhms2 = 0.0001. Discuss with ERM
hrs_lhms_surv3 <- hrs_lhms_surv2 %>%
  mutate(time_lhms2 = ifelse(time_lhms2 < 0 & !is.na(radage), NA, 
                          ifelse(time_lhms2 < 0 & is.na(radage), 0.0001, 
                                 time_lhms2))) %>%
  mutate(lhms_flag = ifelse(time_lhms2 < 0 & !is.na(radage) & lhms_flag==1, NA,
                            lhms_flag))

# checks
# summary(hrs_lhms_surv2$time_lhms2)
# summary(hrs_lhms_surv3$time_lhms2)
# t3 <- hrs_lhms_surv3 %>% distinct(hhidpn, .keep_all = TRUE)
# table(t3$lhms_flag, exclude=NULL) # now 13285 LHMS flag = 1 (19 less, which are the deaths)
# table(t$eos, exclude=NULL)
# table(t3$eos, t3$lhms_flag, exclude=NULL)

#  Non-time-varying data set ------------------------------------------

dat_surv <- hrs_lhms_surv3 %>%
    select(c(hhidpn, hrswave, age_start, age_end, age_end2, starts_with("status"),
             death_flag, eos, rabyear, time, time2, female, meduc_le8, feduc_le8, raceth,
             sbirth, firstiw, lastiw, contains("lhms"), contains("moves"),
             contains("financ"), contains("insec"), hacohort, first_shlt_bin,
             family_financ, radage, child_forced_move, child_hous_insec4,
             raeduc, hhincome_pp, married_partnered, father_unemp,
             first_cesd_bin, first_fnlim, first_fnlim_bin, ravetrn,
             first_father_military, first_father_occup_yr, starts_with("known"),
             everything())) %>%
    group_by(hhidpn) %>%
    mutate(baseline_yr = min(hrswave)) %>%
    filter(hrswave==min(hrswave)) %>%
    ungroup() %>%
    mutate(time_surv = time,
           time_surv2 = time2,
           age_start = as.numeric(age_start),
           age_end = as.numeric(age_end),
           age_end2 = as.numeric(age_end2)) %>%
    mutate(age_start_median = ifelse(age_start <= 60, "50-60", "> 60")) 

# should have 0 obs
check <- dat_surv %>% filter(age_start < 50) # note: 32 obs with age_start = 49.999 
dat_surv %>% filter(agey_e < 50) %>% nrow() 
dat_surv %>% filter(time<=0) %>% nrow()
dat_surv %>% filter(age_end<=50) %>% nrow()

dat_surv %>% filter(time2<=0) %>% nrow() 
dat_surv %>% filter(age_end2<=50) %>% nrow()

dat_surv %>% filter(time_lhms2<=0) %>% nrow() 
dat_surv %>% filter(age_lhms<=50) %>% nrow() # 

summary(dat_surv$age_start) # median = 59.17, was 59.98
table(dat_surv$age_start_median, exclude=NULL)

# CC data set ------------------------------------------------------

### Create flag instead so no data list!

vars <- c("child_forced_move", "family_financ", "age_start", "age_end",
          "female", "raceth", "sbirth", "rabcohort")

dat_surv$complete_flag <- complete.cases(dat_surv[, vars])

# 34954 (was 34,029, 34,032)
dat_surv %>% filter(complete_flag==1) %>% distinct(hhidpn) %>% count()

# Save ----------------------------------------------------------------

saveRDS(dat_surv, here("Aim_1", "data", "processed", "dat_surv.rds"))
