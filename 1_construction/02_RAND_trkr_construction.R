# Purpose: Construct datasets from RAND longitudinal and tracker files
# Author: Taylor M. Mobley
# Date: Apr 20, 2024

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)

#Import RAND and tracker data
rand <- read_sas(paste0("~/Library/CloudStorage/Box-Box/",
                          "HRS_MRG/RANDLongitudinal/randhrs1992_2022v1_SAS/",
                          "randhrs1992_2022v1.sas7bdat"))
trkr <- read_stata(paste0("~/Library/CloudStorage/Box-Box/",
                          "HRS_MRG/HRSRawData/Tracker/trk2022v2/trk2022tr_r.dta"))

# Data filtering and cleaning -------

colnames(rand) <- tolower(colnames(rand))

## Rand data ------
rand_clean <-  rand %>% select(c(hhidpn, rabyear, rabmonth, rabdate,
                                 radyear, radmonth,
                                 raddate, radtimtdth, radsrc,
                                 # age at death
                                 radage_m, radage_y,
                                 hacohort, racohbyr, rabplace,
                                 # select demographics
                                 raracem, rahispan,ragender,raedyrs,raeduc,
                                 rabplace, rameduc, rafeduc, ravetrn,
                                 # age at death in months
                                 radage_m,
                                 # interview status
                                 ends_with("iwstat"),
                                 # interview end date - number of days since 1/1/1960
                                 ends_with("iwbeg"), ends_with("iwmid"), ends_with("iwend"),
                                 # age at end of interview (per RAND guidance)
                                 ends_with("agey_e"), ends_with("agem_e"),
                                 ends_with("agey_b"), ends_with("agem_b"),
                                 ends_with("agey_m"), ends_with("agem_m"),
                                 # marital status
                                 ends_with("mstat"),
                                 # income
                                 ends_with("itot"), ends_with("hhres"),
                                 # depr sx
                                 ends_with("cesd"), ends_with("cesdm"), 
                                 # self-rated health
                                 ends_with("shlt"),
                                 # adls and iadls
                                 ends_with("adl5a"), ends_with("adl6a"),
                                 ## adl items (6) -- ends with a = any, h = get help
                                 # ends_with("batha"), ends_with("dressa"),
                                 # ends_with("eata"), ends_with("beda"), 
                                 # ends_with("walka"), ends_with("toilta"),
                                 # ends_with("bathh"), ends_with("dressh"),
                                 # ends_with("eath"), ends_with("bedh"), 
                                 # ends_with("walkh"), ends_with("toilth"),
                                 # ## iadl items (5) -- ends with a = any, h = get help
                                 # ends_with("phonea"), ends_with("moneya"),
                                 # ends_with("medsa"), ends_with("shopa"),
                                 # ends_with("mealsa"),
                                 # ends_with("phoneh"), ends_with("moneyh"),
                                 # ends_with("medsh"), ends_with("shoph"),
                                 # ends_with("mealsh"),
                                 # other summary indices
                                 ends_with("mobila"), ends_with("grossa"),
                                 ends_with("lgmusa"), ends_with("finea"),
                                 # self-reported health problems
                                 ends_with("heart"), ends_with("strok"),
                                 ends_with("psych"), ends_with("hibp"),
                                 ends_with("diab"), ends_with("cancr"),
                                 ends_with("lung"),
                                 # self-reported health problem Qs
                                 ends_with("heartq"), ends_with("strokq"),
                                 ends_with("psychq"), ends_with("hibpq"),
                                 ends_with("diabq"), ends_with("cancrq"),
                                 ends_with("lungq"))) %>%
  select(-c(starts_with("s"), remstat, respagey_e, reiwend, respagem_e,
            respagem_b, respagey_b, respagem_m, respagey_m)) %>%
  mutate(hhidpn = str_pad(hhidpn,9,pad="0")) %>%
  rename_with(.fn = ~str_remove(., "r10") %>% str_remove(., "h10") %>% paste0(., "10"), .cols = starts_with(c("r10","h10"))) %>% 
  rename_with(.fn = ~str_remove(., "r11") %>% str_remove(., "h11") %>% paste0(., "12"), .cols = starts_with(c("r11","h11"))) %>%
  rename_with(.fn = ~str_remove(., "r12") %>% str_remove(., "h12") %>% paste0(., "14"), .cols = starts_with(c("r12","h12"))) %>%
  rename_with(.fn = ~str_remove(., "r13") %>% str_remove(., "h13") %>% paste0(., "16"), .cols = starts_with(c("r13","h13"))) %>%
  rename_with(.fn = ~str_remove(., "r14") %>% str_remove(., "h14") %>% paste0(., "18"), .cols = starts_with(c("r14","h14"))) %>%
  rename_with(.fn = ~str_remove(., "r15") %>% str_remove(., "h15") %>% paste0(., "20"), .cols = starts_with(c("r15","h15"))) %>%
  rename_with(.fn = ~str_remove(., "r16") %>% str_remove(., "h16") %>% paste0(., "22"), .cols = starts_with(c("r16","h16"))) %>%
  rename_with(.fn = ~str_remove(., "r1") %>% str_remove(., "h1") %>% paste0(., "92"), .cols = starts_with(c("r1","h1"))) %>%
  rename_with(.fn = ~str_remove(., "r2") %>% str_remove(., "h2") %>% paste0(., "94"), .cols = starts_with(c("r2","h2"))) %>%
  rename_with(.fn = ~str_remove(., "r3") %>% str_remove(., "h3") %>% paste0(., "96"), .cols = starts_with(c("r3","h3"))) %>%
  rename_with(.fn = ~str_remove(., "r4") %>% str_remove(., "h4") %>% paste0(., "98"), .cols = starts_with(c("r4","h4"))) %>%
  rename_with(.fn = ~str_remove(., "r5") %>% str_remove(., "h5") %>% paste0(., "00"), .cols = starts_with(c("r5","h5"))) %>%
  rename_with(.fn = ~str_remove(., "r6") %>% str_remove(., "h6") %>% paste0(., "02"), .cols = starts_with(c("r6","h6"))) %>%
  rename_with(.fn = ~str_remove(., "r7") %>% str_remove(., "h7") %>% paste0(., "04"), .cols = starts_with(c("r7","h7"))) %>%
  rename_with(.fn = ~str_remove(., "r8") %>% str_remove(., "h8") %>% paste0(., "06"), .cols = starts_with(c("r8","h8"))) %>%
  rename_with(.fn = ~str_remove(., "r9") %>% str_remove(., "h9") %>% paste0(., "08"), .cols = starts_with(c("r9","h9")))

### transpose to long -----
colnames(rand_clean)
colnames(rand_clean)[!grepl("\\d$", colnames(rand_clean))]

rand_long <- rand_clean %>%
  pivot_longer(cols = -c(hhidpn, rabyear, rabmonth, rabdate, 
                         radyear, radmonth, raddate, radsrc, radtimtdth,
                         radage_m, radage_y, hacohort, racohbyr, rabplace, 
                         raracem, rahispan, ragender, raedyrs, raeduc, ravetrn,
                         rameduc, rafeduc, reiwbeg, reiwmid),
               names_to = c('.value',"hrswave"),
               names_pattern = "(.*?)(\\d+)$") %>%
  # arrange variables
  select(c(hhidpn, hrswave, agey_e, iwstat, everything())) %>%
  mutate(hrswave = ifelse(hacohort %in% c(0,1) & hrswave==94,93,
                          ifelse(hacohort %in% c(0,1) & hrswave==96,95,hrswave))) %>%
  mutate(hrswave = as.numeric(ifelse(hrswave %in% c(92,93,94,95,96,98), 
                                     paste0(19,hrswave), paste0(20,hrswave))),
         rand_flag=1) 

length(unique(rand_long$hhidpn)) # 45234
t <- rand_long %>% filter(is.na(hrswave))

## Tracker data ------
colnames(trkr) <- tolower(colnames(trkr))
trkr_clean <-  trkr %>% select(c(hhid, pn, yrenter, race, hispanic, firstiw,
                                 knowndeceasedmo, knowndeceasedyr,
                                 knowndeceasedsource, exdeathmo, exdeathyr, 
                                 exdodsource, lastalivemo, lastaliveyr, 
                                 lastalivesource, ends_with("age"), 
                                 ends_with("iwwave"))) %>%
  mutate(hhidpn = str_pad(paste(hhid, pn, sep=""),9,pad="0")) %>%
  select(-c(hhid, pn)) 

trkr_long <- trkr_clean %>%
  pivot_longer(cols = -c("hhidpn", "yrenter", "race", "hispanic", "firstiw", 
                         "knowndeceasedmo", "knowndeceasedyr", 
                         "knowndeceasedsource", "exdeathmo", "exdeathyr", 
                         "exdodsource", "lastalivemo", "lastaliveyr", 
                         "lastalivesource"), 
               names_to = c("wave", ".value"),
               names_pattern = "^([a-zA-Z])([a-zA-Z].*)") %>%
  # create hrs wave identifier
  mutate(hrswave = case_when(wave=="a" ~ 1992, wave=="b" ~ 1993, wave=="c" ~ 1994,
                             wave=="d" ~ 1995, wave=="e" ~ 1996, wave=="f" ~ 1998,
                             wave=="g" ~ 2000, wave=="h" ~ 2002, wave=="j" ~ 2004,
                             wave=="k" ~ 2006, wave=="l" ~ 2008, wave=="m" ~ 2010,
                             wave=="n" ~ 2012, wave=="o" ~ 2014, wave=="p" ~ 2016, 
                             wave=="q" ~ 2018, wave=="r" ~ 2020, wave=="s" ~ 2022)) %>%
  mutate(trkr_flag=1) %>%
  select(c(hhidpn, wave, hrswave, everything())) 

length(unique(rand_long$hhidpn)) # 45234
length(unique(trkr_long$hhidpn)) # 46850

# Filter to interviews participated in -----

rand_long <- rand_long %>%
  # filter out rows of data where all covariates are missing
  # filter(if_any(c(mstat:lung), ~ !is.na(.))) %>%
  filter(iwstat==1) 

trkr_long <- trkr_long %>% filter(iwwave==1)

# No one lost in RAND, 1540 lost in trkr (but still more people than rand)

length(unique(rand_long$hhidpn)) # 45234
length(unique(trkr_long$hhidpn)) # 45310

# Merge tracker and rand with RAND as backbone -----
# Do it this way because need covariate data and can now use age + death in RAND through 2022!

rand_trkr_long <- rand_long %>%
  left_join(trkr_long %>% mutate(flag=1), by=c("hhidpn", "hrswave")) %>%
  select(c(hhidpn, hrswave, agey_e, age, iwwave, iwstat, everything())) %>%
  # convert sas dates
  mutate(rabdate = as.Date(rabdate, origin = "1960-01-01"),
         raddate = as.Date(raddate, origin = "1960-01-01"),
         iwbeg = as.Date(iwbeg, origin = "1960-01-01"),
         iwmid = as.Date(iwmid, origin = "1960-01-01"),
         iwend = as.Date(iwend, origin = "1960-01-01"),
         agewave = as.numeric(difftime(iwend, rabdate, units=c("days")))/365.25,
         radage = as.numeric(difftime(raddate, rabdate, units=c("days")))/365.25) %>%
  select(c(hhidpn, hrswave, agewave, radage, everything()))

length(unique(rand_trkr_long$hhidpn)) # 45234
test <- rand_trkr_long %>% filter(flag==1) 
length(unique(test$hhidpn)) # 45233

### Everyone who didn't merge falls into the following categories:
### (1) had no firstiw (firstiw--9996)
### (2) was deceased the same year or before firstiw
### (3) their last known alive year was before their firstiw
# test2 <- trkr_clean %>% filter(!(hhidpn %in% test$hhidpn) & firstiw != 9996)
# test3 <- test2 %>% filter(firstiw < knowndeceasedyr)
# test4 <- test3 %>% filter(firstiw <= lastaliveyr)

## check and remove tracker age >= 996 -----
# only two obs - also missing RAND age. Get rid of these 
summary(rand_trkr_long$agewave)
summary(rand_trkr_long$age) # 996 = birth year not available
check <- rand_trkr_long %>% filter(age==996)
check2 <- check %>% filter(lastaliveyr > hrswave)

rand_trkr_long <- rand_trkr_long %>% filter(age!=996 & !is.na(age))

# lose 1 person
length(unique(rand_trkr_long$hhidpn)) # 45232

# checks
# table(rand_trkr_long$wave, rand_trkr_long$hrswave)

# Save -------------------------------------------------------

saveRDS(rand_trkr_long, here("data", "processed", "rand_trkr_long.RDS"))
