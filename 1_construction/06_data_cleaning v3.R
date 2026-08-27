# Purpose: Post-MI data manipulation
# Author: Taylor M. Mobley
# Date: May 10, 2025

library(here)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(haven)
library(mice)
library(lubridate)

# Load -----
dat <- readRDS(here("data", "processed", "hrs_lhms1998.rds"))

summary(dat$agewave)
summary(dat$agey_b)
summary(dat$agey_e)

# Variable cleaning --------------------------------------------

## Death, health variables -----

# Check tracker and RAND death vars -- no mismatches!
table(dat$knowndeceasedsource, dat$radsrc, exclude=NULL)
table(dat$radmonth, dat$knowndeceasedmo, exclude=NULL)
cor(dat$radyear, dat$knowndeceasedyr, use = "na.or.complete")
summary(dat$radage)
summary(dat$raddate)

hrs_lhms_health <- dat %>%
  group_by(hhidpn) %>%
  fill(lhms_flag, .direction=c("downup")) %>%
  ungroup() %>%
  # count number of obs
  group_by(hhidpn) %>%
  mutate(total_waves= n()) %>%
  # first observed fx limitations and cesd score
  mutate(first_shlt = first(na.omit(shlt)),
         first_cesd = first(na.omit(cesd)),
         first_adl6a = first(na.omit(adl6a)),
         first_iadl5a = first(na.omit(iadl5a)),
         first_mobila = first(na.omit(mobila)),
         first_lgmusa = first(na.omit(lgmusa)),
         first_heart = first(na.omit(heart)),
         first_diab = first(na.omit(diab)),
         first_hibp = first(na.omit(hibp)),
         first_strok = first(na.omit(strok))) %>%
  mutate(first_cesd_bin = case_when(first_cesd >= 4 ~ 1,
                                    first_cesd < 4 & first_cesd >= 0 ~ 0),
         first_fnlim = case_when(is.na(first_adl6a) & is.na(first_iadl5a) ~ NA,
                                 !is.na(first_adl6a) & is.na(first_iadl5a) ~ first_adl6a,
                                 !is.na(first_iadl5a) & is.na(first_adl6a) ~ first_iadl5a,
                                 !is.na(first_iadl5a) & !is.na(first_adl6a) ~ first_iadl5a + first_adl6a),
         first_fnlim_bin = case_when(first_adl6a >= 1 ~ 1,
                                     first_iadl5a >= 1 ~ 1,
                                     first_adl6a == 0 ~ 0,
                                     first_iadl5a == 0 ~ 0),
         first_shlt_bin = case_when(first_shlt %in% c(4,5) ~ 1,
                                    first_shlt %in% c(1,2,3) ~ 0)) %>%
  # ever reported
  mutate(ever_shlt_bin = case_when(all(is.na(shlt)) ~ NA_real_,
                                   any(shlt %in% c(4,5), na.rm=TRUE) ~ 1,
                                   TRUE ~ 0),# ever fair/poor health
         ever_cesd_bin = case_when(all(is.na(cesd)) ~ NA_real_,
                                   any(cesd >= 4, na.rm=TRUE) ~ 1,
                                   TRUE ~ 0), # ever depressive sx
         ever_adl6a_bin = case_when(all(is.na(adl6a)) ~ NA_real_,
                                    any(adl6a >= 1, na.rm=TRUE) ~ 1,
                                    TRUE ~ 0), # ever reported >= 1 adl
         ever_iadl5a_bin = case_when(all(is.na(iadl5a)) ~ NA_real_,
                                     any(iadl5a >= 1, na.rm=TRUE) ~ 1,
                                     TRUE ~ 0), # ever reported >= 1 iadl
         ever_heart = case_when(all(is.na(heart)) ~ NA_real_,
                                any(heart == 1, na.rm=TRUE) ~ 1,
                                TRUE ~ 0),
         ever_diab = case_when(all(is.na(diab)) ~ NA_real_,
                               any(diab == 1, na.rm=TRUE) ~ 1,
                               TRUE ~ 0),
         ever_hibp = case_when(all(is.na(hibp)) ~ NA_real_,
                               any(hibp == 1, na.rm=TRUE) ~ 1,
                               TRUE ~ 0),
         ever_strok = case_when(all(is.na(strok)) ~ NA_real_,
                                any(strok == 1, na.rm=TRUE) ~ 1,
                                TRUE ~ 0)) %>%
  ungroup() %>%
  # death flag 
  mutate(death_flag = ifelse(!is.na(raddate),1,0)) %>%
  # age at censoring
  group_by(hhidpn) %>%
  mutate(censor_age = ifelse(death_flag==0, max(agewave), NA)) %>%
  mutate(hrswave_last = max(hrswave)) %>%
  # end of study categorical variable
  mutate(eos = ifelse(death_flag==1, "death", 
                      ifelse(max(hrswave)==2022, "censored", 
                             "lost to follow-up"))) %>%
  ungroup() 

## Covariate cleaning -----
# summary(hrs_lhms_health$rabyear) # 1892-1973 (because we have 2022 here)

hrs_lhms_covars <- hrs_lhms_health %>%
    select(c(hhidpn, firstiw, everything())) %>%
    mutate(first_adl6a = as.numeric(first_adl6a),
           first_iadl5a = as.numeric(first_iadl5a),
           rameduc = as.numeric(rameduc),
           rafeduc = as.numeric(rafeduc)) %>%
    mutate(female = case_when(ragender==2 ~ 1, ragender==1 ~ 0),
           meduc_le8 =
             ifelse(rameduc <= 8 | is.na(rameduc), 1, ifelse(rameduc > 8, 0, NA)),
           feduc_le8 =
             ifelse(rafeduc <= 8 | is.na(rafeduc), 1, ifelse(rafeduc > 8, 0, NA)),
           meduc3 = factor(case_when(is.na(rameduc) ~ 2, 
                                     rameduc <= 8 ~ 1, 
                                     rameduc > 8 ~ 0)),
           feduc3 = factor(case_when(is.na(rafeduc) ~ 2,
                                     rafeduc <= 8 ~ 1,
                                     rafeduc > 8 ~ 0)),
           child_forced_move = ifelse(first_forced_move %in% c(8,9),NA,
                                      ifelse(first_forced_move==1,1,
                                             ifelse(first_forced_move==5,0,
                                                    first_forced_move))),
           sbirth = case_when(rabplace %in% c(5,6,7) ~ 1, # yes
                              rabplace %in% c(1,2,3,4,8,9,10,11,12,13) ~ 0), # no or not US
           usbirth = case_when(rabplace %in% c(11,13) ~ 1,
                               rabplace %in% c(1,2,3,4,5,6,7,8,9,10,12) ~ 0),
           family_financ = case_when(first_family_financ == 1 ~ "Pretty well off",
                                     first_family_financ == 3 ~ "Average",
                                     first_family_financ == 5 ~ "Poor",
                                     first_family_financ == 6 ~ "Varied"),
           poor_family_financ = ifelse(family_financ=="Poor", 1, 0),
           child_hous_insec4 = case_when(poor_family_financ==1 & child_forced_move==1 ~ 3,
                                         poor_family_financ==0 & child_forced_move==1 ~ 2,
                                         poor_family_financ==1 & child_forced_move==0 ~ 1,
                                         poor_family_financ==0 & child_forced_move==0 ~ 0),
           child_hous_insec2 = ifelse(poor_family_financ==1 & child_forced_move==1, 1, 0),
           rabcohort = case_when(rabyear >= 1883 & rabyear <= 1927 ~ "Born 1892-1927", # Lost and Greatest Gens
                                 rabyear >= 1928 & rabyear <= 1945 ~ "Born 1928-1945", # Silent Gen
                                 rabyear >= 1946 & rabyear <= 1964 ~ "Born 1946-1964", # Baby Boomers
                                 rabyear >= 1965 & rabyear <= 1980 ~ "Born 1965-1973"), # Gen X
           hhincome_pp = itot/sqrt(hhres),
           married_partnered = case_when(mstat %in% c(1,2,3) ~ 1,
                                         mstat %in% c(4:8) ~ 0,
                                         is.na(mstat) ~ NA),
           father_unemp = case_when(first_father_unemp==5 ~ 0,
                                    first_father_unemp==1 ~ 1,
                                    first_father_unemp==6 ~ 2,
                                    first_father_unemp==7 ~ 3),
           raceth = case_when((rahispan!=1 | is.na(rahispan)) & raracem==2 ~ 2,
                              (rahispan!=1 | is.na(rahispan)) & raracem==3 ~ 3,
                              (rahispan!=1 | is.na(rahispan)) & raracem==1 ~ 4,
                              rahispan==1 ~ 1)) %>%
  # LHMS year, age, est date
  mutate(lhms_year = ifelse(is.na(lhms_flag), NA,
                            as.numeric(gsub(".*?(\\d{4}).*", "\\1", lhmswind)))) %>%
  select(c(hhidpn, hrswave, firstiw, hrswave_last, radyear, radage, radage_y, 
           lhms_year, eos, everything()))

### Estimate LHMS date ----
# From https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/1739814830/LHMS1519_DD.pdf
# Wave        Field Period 
# 2015 Fall   Dec 2015 – Aug 2016, middate = April 2016
# 2017 Spring Jun 2017 – Dec 2017, middate = Sep 2017
# 2017 Fall   Dec 2017 – Jun 2018, middate = March 2018 
# 2019 Spring Jun 2019 – Dec 2019, middate = September 2019 
# 2019 Fall   Oct 2019 – Mar 2020, middate = Jan 2020

# 2015
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/2015LHMS_data_description.pdf
# "In December 2015, questionnaires were mailed to a subsample of HRS respondents (n= 11,256).
# The sample for the 2015 LHMS consists of all living HRS respondents who were not included in
# the 2015 Consumption and Activities Mail Survey (CAMS) and who completed their most recent
# HRS core interview in English (rather than Spanish). The field period for the 2015 LHMS was
# December 2015 through summer 2016.

# 2017 Spring 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/1675374681/2017LHMS_Spring_data_description_V3_Jan2023.pdf
# In June 2017, questionnaires were mailed to a subsample of HRS respondents (n= 5,174). The
# sample for the 2017 LHMS was drawn from the 2015 Consumption and Activities Mail Survey 3
# (CAMS) sample. It included 2015 CAMS sample members who were still alive in 2017 and whose
# household was considered finalized on their 2016 core interview(s) by early March 2017.
# (Members of finalized households had either completed core interviews or were considered final
# refusals for the core that wave.) A small number of respondents who were in the sample for the
# 2016 Harmonized Cognitive Assessment Protocol (HCAP) and who had not completed their
# assessment by early March, 2017 were excluded. New spouses of eligible panel respondents were
# also included in the 2017 LHMS sample. The field period for the LHMS 2017 Spring was June through December 2017.

# 2017 Fall 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/1688072320/2017LHMS_Fall_DataDescription_V2.pdf
# The field period for the LHMS 2017 Fall was December 2017 through June 2018. Questionnaires
# were mailed to a subsample of HRS respondents (n= 5,180). The data file contains 559 variables
# and data for 1,445 respondents. The response rate is 28% percent.

# 2019 Spring 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/1703112278/2019LHMS_Spring_data_description_Release3.pdf
# In June 2019, questionnaires were mailed to a subsample of HRS respondents (n= 1748). The
# sample for the 2019 Spring LHMS consisted of HRS respondents who are part of the CAMS sample
# and who had not previously completed the LHMS in 2017. This includes persons who were asked
# but did not respond to the 2017 CAMS survey and participants from the late-Baby Boomer (LBB)
# birth cohort who entered HRS in 2016 but were not assigned to the LHMS in 2017.

# 2019 Fall 
# https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/1714589806/2019LHMS_Fall_Long_DD__Public_042624_Release2.pdf
# In October 2019, questionnaires were mailed to a subsample of HRS respondents (n= 6966). The
# sample for the 2019 Fall LHMS included three groups of HRS respondents. The first group
# consisted of 2865 HRS respondents from the late-Baby Boomer cohort (LBB) who had not
# previously been invited to participate in the LHMS. The second group (n=3143) were HRS
# respondents who had been invited previously but had never returned a LHMS questionnaire. The
# third group was made up of 958 HRS respondents who had completed the 2015 LHMS but not the
# 2017 Supplemental LHMS.

# Conclusion: Use inital field date for now -- confirmed with ERM on 3/5/26

hrs_lhms_covars_clean <- hrs_lhms_covars %>%
  mutate(lhms_month = case_when(is.na(lhms_flag) ~ NA,
                                lhms_year == 2015 ~ 12,
                                lhms_year == 2017 & str_detect(lhmswind, "2017 SPRING") ~ 6,
                                lhms_year == 2017 & str_detect(lhmswind, "FALL FULL") ~ 12,
                                lhms_year == 2019 & str_detect(lhmswind, "2019 SPRING") ~ 6,
                                lhms_year == 2019 & str_detect(lhmswind, "2019 FALL FULL") ~ 10),
  lhms_date = ymd(paste(lhms_year,lhms_month,1), quiet=TRUE), # uses 1st of the month
  age_lhms = ifelse(is.na(lhms_year), NA, time_length(interval(rabdate, lhms_date), "years"))) %>%
  # Difference between LHMS year and last IW
  mutate(lhms_endyr_diff = ifelse(!is.na(radyear), radyear - lhms_year,
                                  hrswave_last - lhms_year)) %>%
  # Flag for death age occurring before LHMS
  mutate(temp = ifelse(radage < age_lhms & lhms_flag==1, 1, NA)) %>%
  select(c(hhidpn, hrswave, firstiw, hrswave_last, radyear, radage, radage_y, 
           lhms_year, rabdate, raddate, lhms_date, radsrc, age_lhms, temp, eos,
           lh61, everything()))

# 19 people seem to have died before LHMS, 10 had proxy fill out survey
table(hrs_lhms_covars_clean$temp, hrs_lhms_covars_clean$lhms_flag, exclude=NULL)
t <- hrs_lhms_covars_clean %>% filter(temp==1) %>% distinct(hhidpn, .keep_all = TRUE)
table(t$lh61, t$lh61_fallsupp, exclude=NULL)
length(unique(t$hhidpn)) # 19 people

# Holding off for now - discuss with ERM
# For ease, I'm assuming death age is correct and am changing lhms flag, date, and time
# to missing when participant death age earlier than LHMS age
# hrs_lhms_covars_clean <- hrs_lhms_covars_clean %>%
#   mutate(lhms_flag = ifelse(is.na(temp) & lhms_flag==1, 1,
#                             ifelse(temp==1 & lhms_flag==1, NA, lhms_flag)),
#          lhms_date = ifelse(is.na(temp) & !is.na(lhms_date), lhms_date,
#                             ifelse(temp==1 & !is.na(lhms_date), NA, lhms_date)),
#          age_lhms = ifelse(is.na(temp) & !is.na(age_lhms), age_lhms, 
#                            ifelse(temp==1 & !is.na(age_lhms), NA, age_lhms)))

# checks
# table(hrs_lhms_covars_clean$lhms_flag, hrs_lhms_covars_clean$temp, exclude=NULL)
# table(hrs_lhms_covars_clean$lhms_date, hrs_lhms_covars_clean$temp, exclude=NULL)
# tapply(hrs_lhms_covars_clean$age_lhms, hrs_lhms_covars_clean$temp, summary)

## High childhood moves at 90th percentile in cc sample -----
dat_cc <- hrs_lhms_covars_clean %>% filter(lhms_flag==1) %>% distinct(hhidpn, .keep_all=TRUE) 
dat_cc <- dat_cc[complete.cases(dat_cc[, c("childhood_moves_v2")]), ]
dat_cc <- dat_cc[complete.cases(dat_cc[, c("child_forced_move")]), ]
dat_cc <- dat_cc[complete.cases(dat_cc[, c("raceth", "female", "sbirth")]), ]
quantile(dat_cc$childhood_moves_v2, probs = c(0.10, 0.25, 0.5, 0.75, 0.9))

## Childhood res mobility cleaning -----

hrs_lhms_clean <- hrs_lhms_covars_clean %>%
  mutate(high_childhood_moves_v2 = case_when(childhood_moves_v2 >= quantile(dat_cc$childhood_moves_v2, probs = c(0.9)) ~ 1,
                                             childhood_moves_v2 < quantile(dat_cc$childhood_moves_v2, probs = c(0.9)) ~ 0),
         high_childhood_moves3_v2 = as.factor(case_when(childhood_moves_v2 >= quantile(dat_cc$childhood_moves_v2, probs = c(0.9)) ~ 2,
                                                        childhood_moves_v2 == 0 ~ 0,
                                                        childhood_moves_v2 < quantile(dat_cc$childhood_moves_v2, probs = c(0.9)) ~ 1))) %>%
    mutate(child_move_insec4 = case_when(child_forced_move==1 & high_childhood_moves_v2==1 ~ 4,
                                         child_forced_move==1 & high_childhood_moves_v2==0 ~ 3,
                                         child_forced_move==0 & high_childhood_moves_v2==1 ~ 2,
                                         child_forced_move==0 & high_childhood_moves_v2==0 ~ 1))

# Saving cleaned data -----

saveRDS(hrs_lhms_clean, file=here("data", "processed", "hrs_lhms_clean.rds"))
