# Purpose: Construct datasets from RAND fat files, LHMS, and RAND/trkr longitudinal data
# Author: Taylor M. Mobley
# Date: Feb 11, 2024
# Last updated: 

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)

# Import ---------------------------------------------------

# Import HRS LHMS data -- new dataset run by TMM 2/10/2026
lhms <- read_sas(here("data", "rawdata", "LHMS_Aggregate-2", "lhms1519a_r.sas7bdat"))
colnames(lhms) <- tolower(colnames(lhms)) 

# Import RAND long data set & trkr clean data
rand_trkr_long <- readRDS(here("data", "processed", "rand_trkr_long.RDS"))

# HRS fatfiles: residence and move variables
hrsfatfiles_join <- readRDS(here("data", "processed", "hrsfatfiles_join.rds"))

# Transform and clean fat file data ----------------------------
hrsfatfiles_long <- hrsfatfiles_join %>%
  # remove extra vars -- variables that were selected from patterns but that 
  ## we don't need
  select(-c(llb040, mlb040, nlb040, h004, rlb039)) %>%
  # first rename 1995-2000 variables to match 2002-2020 var name patterns
  rename(dh002 = d2225,     eh002 = e2225,
         dh004 = d2226,     eh004 = e2226,
         dh014 = d2234,     eh014 = e2234,
         db039 = d698,      eb039 = e698,
         db040 = d697,      eb040 = e697,
         ex033 = e88,
         db041m1 = d702m1,  eb041m1 = e702m1,
         db041m2 = d702m2,  eb041m2 = e702m2,
         dh148 = d2387,      eh148 = e2387,
         dh149 = d2394,     eh149 = e2394,
         dh050 = d2395,     eh050 = e2395,
         
         fh002 = f2742,     gh002 = g3060,
         fh004 = f2743,     gh004 = g3061,
         fh014 = f2751,     gh014 = g3069,
         fb039 = f1017,     gb039 = g1104,
         fb040 = f1018,     gb040 = g1105,
         fx033 = f56,       gx033 = g56,
         fb041m1 = f1022m1, gb041m1 = g1109m1,
         fb041m2 = f1022m2, gb041m2 = g1109m2,
         fh148 = f2904,      gh148 = g3222,
         fh149 = f2911,     gh149 = g3229,
         fh050 = f2912,     gh050 = g3230,
         
         fb020 = f993,      fb021 = f994,
         gb020 = g1080,     gb021 = g1081,
         fb023 = f996,      gb023 = g1083) %>%
  select(-c(contains("702m"), contains("f1022m"), contains("g1109m"), 
            ends_with("lb023"))) %>%
  pivot_longer(cols = -c("hhidpn"), 
               names_to = c("wave", ".value"),
               names_pattern = "^([a-zA-Z])[a-z](\\d.*)") %>%
  filter(!is.na(wave)) %>%
  # create hrs wave identifier
  mutate(hrswave = case_when(wave=="d" ~ 1995, wave=="e" ~ 1996, wave=="f" ~ 1998,
                             wave=="g" ~ 2000, wave=="h" ~ 2002, wave=="j" ~ 2004,
                             wave=="k" ~ 2006, wave=="l" ~ 2008, wave=="m" ~ 2010,
                             wave=="n" ~ 2012, wave=="o" ~ 2014, wave=="p" ~ 2016, 
                             wave=="q" ~ 2018, wave=="r" ~ 2020, wave=="s" ~ 2022))

## renaming variables before transforming long -----
hrsfatfiles_long_clean <-  hrsfatfiles_long %>% 
  # rename variables
  rename(dwelling_type = `002`, dwelling_tenure = `004`, mh_tenure = `014`, 
         yr_resid = `039`, mn_resid = `040`, whether_moved = `033`,
         reason_move1 = `041m1`, reason_move2 = `041m2`,
         rate_home = `148`, make_access = `149`, rate_neighborhood = `050`, 
         family_financ = `020`, child_forced_move = `021`,
         father_unemp = `023`, father_occup = `024m`) %>%
  # calculate resid start date
  mutate(resid_date = as.Date(ifelse(yr_resid > 1900 & 
                                       yr_resid < 2030 & 
                                       !is.na(mn_resid), 
                      paste(yr_resid, mn_resid, 15, sep="-"),NA))) %>%
  arrange(hhidpn, hrswave) 

# check how many rows where all covars are missing
# check <- hrsfatfiles_long_clean %>%
#   filter(if_any(-c(hhidpn, wave, hrswave), ~ !is.na(.)))
# 
# length(unique(check$hhidpn))
# length(unique(hrsfatfiles_long_clean$hhidpn))

hrsfatfiles_long_clean <- hrsfatfiles_long_clean %>%
  filter(if_any(-c(hhidpn, wave, hrswave), ~ !is.na(.))) %>%
  mutate(hrsff_flag=1) %>%
  select(c(hhidpn, hrswave, wave, everything()))

# table(hrsfatfiles_long_clean$hrswave, exclude = NULL)

# merge HRS resid data with tracker and rand data -------------

length(unique(hrsfatfiles_long_clean$hhidpn)) # 42,837
length(unique(rand_trkr_long$hhidpn)) # 45,232

hrsff_rand_trkr_long <- rand_trkr_long %>%
  mutate(trkr_rand_flag=1) %>%
  left_join(hrsfatfiles_long_clean, by=c("hhidpn", "hrswave")) %>% 
  select(hhidpn, everything())

# check merge -- 32519 personwaves don't merge. most of this is 1992/1994 waves
# and only 1 person doesn't merge
# table(hrsff_rand_trkr_long$hrsff_flag, exclude=NULL)
# check <- hrsff_rand_trkr_long %>% filter(is.na(hrsff_flag))
# table(check$hrswave)
# check <- check %>% filter(hrswave >=1995)
# check <- hrsfatfiles_long_clean %>%
#   filter(!(hhidpn %in% hrsff_rand_trkr_long$hhidpn))

# clean lhms data ----------------------------------------------

# filter lhms residential variables of interest
lhms_resid <- lhms %>% 
  mutate(hhidpn = str_pad(paste(hhid, pn, sep=""),9,pad="0")) %>%
  select(c(hhidpn, lhms, lhmswind, lh61, lh61_fallsupp, lh4e, lh9, lh13, lh13a, lh16, lh20, 
           # Atypical contexts before/after age 16
           lh2a, lh2b, lh2c, lh2d, lh2e, lh2f, lh2g, lh2h, lh2i, 
           contains("lh5_"))) %>%
  mutate(lhms = case_when(lhms == 1 ~ "2015 AND/OR 2017 FALL SUPP/2019 FALL SUPP",
                          lhms == 2 ~ "LHMS 2017 SPRING",
                          lhms == 3 ~ "LHMS 2017 FALL FULL",
                          lhms == 4 ~ "LHMS 2019 SPRING",
                          lhms == 5 ~ "LHMS 2019 FALL FULL"),
         lhmswind = case_when(lhmswind == 1 ~ "COMPLETED 2015 AND 2017 FALL SUPP INTERVIEWS",
                              lhmswind == 2 ~ "COMPLETED 2015 AND 2019 FALL SUPP INTERVIEWS",
                              lhmswind == 3 ~ "COMPLETED 2015 INTERVIEW BUT NOT 2017 OR 2019 FALL SUPP INTERVIEW",
                              lhmswind == 4 ~ "COMPLETED 2017 FALL SUPP BUT  WAS NOT IN THE  2015 INTERVIEWED SAMPLE",
                              lhmswind == 5 ~ "COMPLETED 2017 SPRING INTERVIEW",
                              lhmswind == 6 ~ "COMPLETED 2017 FALL FULL INTERVIEW",
                              lhmswind == 7 ~ "COMPLETED 2019 SPRING INTERVIEW",
                              lhmswind == 8 ~ "COMPLETED 2019 FALL FULL INTERVIEW"),
         lhms_status = case_when(lh61==1 ~ 1,
                                 lh61==2 ~ 2,
                                 lh61==3 ~ 3,
                                 lh61_fallsupp==1 ~ 1,
                                 lh61_fallsupp==2 ~ 2,
                                 lh61_fallsupp==3 ~ 3))

# check n's
length(unique(lhms_resid$hhidpn))

# check n again -- no one shows up twice!
# note: same n (14936) from when I pulled in 2019 surveys separately!
length(unique(lhms_resid$hhidpn)) == nrow(lhms_resid)

# Merge hrs core wave + rand/trkr data with lhms data -----

# Merge with long hrs data as "backbone"
lhms_hrs_long <- hrsff_rand_trkr_long %>%
  left_join(lhms_resid %>% mutate(lhms_flag=1), by=c("hhidpn")) %>%
  select(c(hhidpn, everything()))

length(unique(hrsff_rand_trkr_long$hhidpn)) # 45232
length(unique(lhms_hrs_long$hhidpn)) # 45232

# 14,911 lhms people merge, so 25 don't. checking this below!
# 
# table(lhms_hrs_long$lhms_flag, exclude=NULL) # 143465 obs
# check <- lhms_hrs_long %>% filter(lhms_flag==1) %>% distinct(hhidpn, .keep_all = TRUE)
# 
# # 25 people who did not merge entered study but never did intverview (firstiw code=9996)
# check <- lhms_resid %>%
#   anti_join(hrsff_rand_trkr_long, by=c("hhidpn"))
# trkr <- read_stata(paste0("/Users/taylormobley/Library/CloudStorage/Box-Box/",
#                           "HRS_MRG/HRSRawData/Tracker/trk2020tr_r.dta")) %>%
#   select(c(HHID, PN, YRENTER, FIRSTIW)) %>%
#   mutate(hhidpn = str_pad(paste(HHID, PN, sep=""),9,pad="0")) %>%
#   filter(hhidpn %in% check$hhidpn)
# check2 <- trkr %>% filter((hhidpn %in% check$hhidpn))

# save data set ----------------------------------------------
saveRDS(lhms_hrs_long, here("data", "processed", "lhms_hrs_long.RDS"))
