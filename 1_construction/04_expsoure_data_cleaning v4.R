# Purpose: Clean exposure data
# Author: Taylor M. Mobley
# Date: Apr 20, 2024

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)
library(gtsummary)

# LHMS 2015/2017/2019 codebook
# https://hrs.isr.umich.edu/sites/default/files/meta/xyear/lhms/codebook/lhms1519a_ri.htm
# Import ---------------------------------------------------
lhms_hrs_long <- readRDS(here("data", "processed", "lhms_hrs_long.rds")) 

length(unique(lhms_hrs_long$hhidpn)) # 45232 people

# Set lhms_flag to missing if only 2017 fall supp completed
table(lhms_hrs_long$lhmswind, lhms_hrs_long$lhms_flag, exclude=NULL)

lhms_hrs_long <- lhms_hrs_long %>%
  mutate(lhms_flag = ifelse(lhmswind == 
                              "COMPLETED 2017 FALL SUPP BUT  WAS NOT IN THE  2015 INTERVIEWED SAMPLE", 
                            NA, lhms_flag))

table(lhms_hrs_long$lhmswind, lhms_hrs_long$lhms_flag, exclude=NULL)

# Death source distribution
# test <- lhms_hrs_long %>% group_by(hhidpn) %>% filter(hrswave==min(hrswave)) %>% ungroup()
# table(test$knowndeceasedsource, test$radsrc, exclude=NULL)

# Should get 14,911 lhms people who merged with trkr data 
lhms_hrs_resid <- lhms_hrs_long %>% 
  # remove people who did not participate in lhms
  filter(!is.na(lhms)) %>%
  group_by(hhidpn) %>%
  # keep baseline HRS wave for each person
  filter(hrswave==min(hrswave)) %>%
  ungroup() 

# check <- lhms_hrs_long %>% filter(!(hhidpn %in% lhms_hrs_resid$hhidpn))
# remove people who only particpated in 2017 lhms fall supp 
# and thus not asked residential history questions -- should be n=2 
table(lhms_hrs_resid$lhmswind,lhms_hrs_resid$lhms_flag, exclude=NULL)

lhms_hrs_resid <- lhms_hrs_resid %>%
  filter(lhmswind != "COMPLETED 2017 FALL SUPP BUT  WAS NOT IN THE  2015 INTERVIEWED SAMPLE")

# 14,909 people
length(unique(lhms_hrs_resid$hhidpn))

# Cleaning residential mobility data ----------------------------------

lhms_hrs_resid_long <- lhms_hrs_resid %>%
  select(c(hhidpn, rabyear, starts_with("lh5"))) %>%
  pivot_longer(cols = c(starts_with("lh5")), 
               names_to = c(".value", "response", ".value"),
               names_pattern = "(^[^_]+(?=_))_(\\d+)([a-z]*)") %>%
  rename(resid_year = lh5a,
         resid_location = lh5b,
         resid_tenure = lh5c) %>%
  # any reported residence change information
  filter(if_any(c(resid_year, resid_location, resid_tenure), ~ !is.na(.))) %>%
  mutate(resid_age = ifelse(is.na(resid_year), NA, resid_year - rabyear),
         resid_own = ifelse(is.na(resid_tenure), NA,
                          ifelse(resid_tenure==1 ,1, 0)),
         resid_rent = ifelse(is.na(resid_tenure), NA,
                             ifelse(resid_tenure==2 ,1, 0)),
         resid_tenure = case_when(resid_tenure==1 ~ "own",
                                  resid_tenure==2 ~ "rent",
                                  resid_tenure==7 ~ "other", 
                                  resid_tenure==8 ~ "dk",
                                  is.na(resid_tenure) ~ NA))

# 165 people report residences after birth (194 obs)
length(unique(lhms_hrs_resid_long$hhidpn)) # 13859 people
test <- lhms_hrs_resid_long %>% filter(rabyear > resid_year)
length(unique(test$hhidpn))

# Checking what happens if we drop obs where residence reported before birth
# lost 33 people -- they all have 1 response. They should have 0 moves, not missing!
# I will keep them in and calculate moves starting at rabyear 
# check <- lhms_hrs_resid_long %>% filter(rabyear <= resid_year | is.na(resid_year))
# length(unique(check$hhidpn)) # 13825 people
# check <- lhms_hrs_resid_long %>% filter(!(hhidpn %in% check$hhidpn))

lhms_hrs_resid_long <- lhms_hrs_resid_long %>%
  mutate(resid_yr_miss = case_when(is.na(resid_year) ~ 1,
                               !is.na(resid_year) ~ 0)) %>%
  group_by(hhidpn) %>%
  ## redefine response var because we dropped some obs
  mutate(response = row_number()) %>%
  ## create a flag that tells us if people have any vs no missingnesss in resid_year
  mutate(any_yr_miss = max(resid_yr_miss),
         resid_age_start = first(na.omit(resid_age)),
         resid_year_start = first(na.omit(resid_year))) %>%
  ## create another flag if all yrs are missing
  mutate(all_yr_miss = as.integer(all(is.na(resid_year)))) %>%
  ungroup() 

# n = 13859 people who reported any residential data
length(unique(lhms_hrs_resid_long$hhidpn)) 

# looking at missingness:
## 4234 obs missing resid_year (out of 96828 obs)
table(lhms_hrs_resid_long$resid_yr_miss, exclude=NULL)
## 1565 people have some missingness, 12293 people do not (11.3% missingness)
lhms_hrs_resid_long %>% filter(response==1) %>% 
  group_by(any_yr_miss) %>% count()

# 13393 people reporting resid_year at any point (both checks same)
check <- lhms_hrs_resid_long %>% filter(!is.na(resid_year))
check <- lhms_hrs_resid_long %>% filter(all_yr_miss==0)
length(unique(check$hhidpn))

# New, new approach -----------------------------------------------

## Create 2 exposure versions: 
## 1: take everyone who with at least one resid year value and back fill
### ages when available, assuming responses were filled in chronological order
## 2: "complete case" approach -- only count people who have no missng resid 
### yrs. I need resid year so that I can approximate age at move

lhms_hrs_no_yr_miss <- lhms_hrs_resid_long %>% filter(any_yr_miss==0)
lhms_hrs_start_birth <- lhms_hrs_resid_long %>% filter(resid_age_start==0)

# first check n's
length(unique(lhms_hrs_no_yr_miss$hhidpn)) # 12294
length(unique(lhms_hrs_start_birth$hhidpn)) # 12311

# check overlap -- from 90061 to 82610 obs
check <- lhms_hrs_start_birth %>% filter(hhidpn %in% lhms_hrs_no_yr_miss$hhidpn)
length(unique(check$hhidpn)) # 11389 people

lhms_hrs_resid_fill <- lhms_hrs_resid_long %>%
  filter(all_yr_miss==0) %>%
  arrange(hhidpn, response) %>%
  group_by(hhidpn) %>%
  # filling backward - this takes the last reported age and fills in for prev moves
  fill(resid_age, .direction=c("up")) %>%
  fill(resid_year, .direction=c("up")) %>%
  ungroup()

# 1505 missing values vs. 4234
summary(lhms_hrs_resid_fill$resid_year)
summary(lhms_hrs_resid_long$resid_year) 

# now, recount responses
lhms_hrs_resid_fill <- lhms_hrs_resid_fill %>%
  group_by(hhidpn) %>% 
  # rearrange by id and resid_year (some our of order)
  arrange(hhidpn, resid_year, response) %>%
  mutate(response_v1 = row_number()) %>%
  # recalculate start age
  mutate(resid_age_start_v1 = first(na.omit(resid_age)),
         resid_age_end_v1 = last(na.omit(resid_age))) %>%
  ungroup() %>%
  # Rename variables for calculations in next section
  rename(resid_age_v1 = resid_age,
         resid_year_v1 = resid_year) %>%
  select(c(hhidpn, rabyear, response_v1, resid_year_v1, resid_age_v1, 
           resid_age_start_v1, resid_age_end_v1))

# In this "complete case" approach, we will drop 1560 people out of 13853 (11.3% missingness)
lhms_hrs_resid_cc <- lhms_hrs_resid_long %>% 
  filter(any_yr_miss==0) %>%
  # rearrange by resid_year (some are out of order)
  arrange(hhidpn, resid_year, response) %>%
  group_by(hhidpn) %>%
  mutate(response_v2 = row_number()) %>%
  mutate(resid_age_start_v2 = min(resid_age, na.rm = TRUE),
         resid_age_end_v2 = max(resid_age, na.rm = TRUE)) %>%
  ungroup() %>%
  # Rename variables for calculations in next section
  rename(resid_age_v2 = resid_age,
         resid_year_v2 = resid_year) %>%
  select(c(hhidpn, rabyear, response_v2, resid_year_v2, resid_age_v2,
           resid_age_start_v2, resid_age_end_v2))

# Check n's
# 13393 people
length(unique(lhms_hrs_resid_fill$hhidpn))
# 12294 people -- previously, 11377 people when I required 
# no missing year AND start at age 0
length(unique(lhms_hrs_resid_cc$hhidpn))

# Everyone in CC case is in the fill case (makes sense!)
check <- lhms_hrs_resid_cc %>% filter(!(hhidpn %in% lhms_hrs_resid_fill$hhidpn))
length(unique(check$hhidpn)) 

# Calculate exposures! ---------------------------------------

# Note: TMM holding off with this approach for now
# lhms_hrs_fill <- lhms_hrs_resid_fill %>%
#   group_by(hhidpn) %>%
#   # If resid_age starts with 0, need to subtract 1, otherwise count all as moves
#   mutate(total_moves_v1 = n()) %>%
#   mutate(total_moves_v1 = ifelse(resid_age_start_v1<=0, total_moves_v1 - 1, total_moves_v1)) %>%
#   fill(total_moves_v1) %>% 
#   ungroup()
# 
#   mutate(childhood_moves_v1 = ifelse(total_moves_v1==0, 0,
#                                      sum(resid_age_v1 < 17, na.rm=TRUE) - 1),
#          adulthood_moves_v1 = total_moves_v1 - childhood_moves_v1) %>%
# 
#   fill(childhood_moves_v1) %>%
#   fill(adulthood_moves_v1) %>%
#   ungroup()

lhms_hrs_cc_all <- lhms_hrs_resid_cc %>%
  group_by(hhidpn) %>%
  # If resid_age starts with 0, need to subtract 1, otherwise count all as moves
  mutate(total_moves_v2 = ifelse(resid_age_start_v2 <= 0, n() - 1, n()),
         childhood_moves_v2 = 
           ifelse(total_moves_v2==0, 0, 
                  ifelse(resid_age_start_v2 <= 0,
                         sum(resid_age_v2 < 17, na.rm = TRUE) - 1,
                         sum(resid_age_v2 < 17, na.rm = TRUE) 
                  ))
  ) %>%
  mutate(adulthood_moves_v2 = total_moves_v2 - childhood_moves_v2) %>%
  fill(total_moves_v2) %>% 
  fill(childhood_moves_v2) %>%
  ungroup()

# quantile(lhms_hrs_cc_all$childhood_moves_v2, probs=seq(0,1,0.1))
# test <- lhms_hrs_cc_all %>% filter(resid_age_start_v2 < 0) 

# Create residential instability measures -----
childhood_mobility <- lhms_hrs_cc_all %>%
  filter(resid_age_v2 < 17) %>%
  arrange(hhidpn, resid_year_v2) %>%
  group_by(hhidpn) %>%
  mutate(move_yrs = resid_year_v2 - lag(resid_year_v2),
         cum_move_yrs = {
           mv = move_yrs
           mv_no_na = replace_na(mv, 0)
           cs = cumsum(mv_no_na)
           cs[is.na(mv)] = NA
           cs}
  ) %>%
  # mutate(cum_move_yrs = ifelse(is.na(cum_move_yrs), move_yrs, cum_move_yrs)) %>%
  mutate(fast_move = ifelse(move_yrs >= 0 & move_yrs < 2, 1, 0),
         total_fast_moves = sum(fast_move, na.rm=TRUE),
         run_id = data.table::rleid(fast_move),  # Assigns a unique ID to each streak of equal values
         consec_fast_moves = if_else(fast_move == 1, ave(fast_move, run_id, FUN = seq_along), 0L)
  ) %>%
  mutate(consec_fast_moves = ifelse(any(!is.na(consec_fast_moves)), 
                                    max(consec_fast_moves, na.rm=TRUE), 0)) %>%
  ungroup() %>%
  select(-run_id)

# one response per indivudal + add in participants with 0 childhood moves
mobility_resp1 <- childhood_mobility %>%
  group_by(hhidpn) %>%
  filter(response_v2==1) %>%
  ungroup() %>%
  plyr::rbind.fill(lhms_hrs_cc_all %>% 
                     filter(!(hhidpn %in% childhood_mobility$hhidpn)) %>%
                     mutate(fast_move = 0,
                            total_fast_moves = 0,
                            consec_fast_moves = 0)) 

# 90th percentile both 1, go with at least 2 
quantile(mobility_resp1$total_fast_moves, probs=seq(0,1,0.1))
quantile(mobility_resp1$consec_fast_moves, probs=seq(0,1,0.1))

## V1: moved in back-to-back years > 1 time -----
## V2: > 1 moves in back-to-back years -----
## V3: multiple moves in a 5 year span -----
childhood_instab <- childhood_mobility %>%
  group_by(hhidpn) %>%
  mutate(child_res_instab = ifelse(total_fast_moves >= 2, 1, 0),
         child_res_instab_v2 = ifelse(consec_fast_moves >= 2, 1, 0),
         child_res_instab_v3 =  map_int(row_number(), function(i) {
           current_year <- resid_year_v2[i]
           # Count how many moves in [current_year - 4, current_year]
           # Exclude current row (i) if you want only *other* moves
           # moves_in_window <- sum(resid_year_v2 >= (current_year - 4) & resid_year_v2 <= current_year, na.rm = TRUE)
           # Or exclude current row like this:
           moves_in_window <- sum(resid_year_v2 >= (current_year - 4) & resid_year_v2 <= current_year, na.rm = TRUE) - 1
           as.integer(moves_in_window >= 2)
         }
         )
  ) %>%
  mutate(child_res_instab_v3 = ifelse(child_res_instab_v3==0,NA,child_res_instab_v3)) %>%
  fill(child_res_instab_v2, child_res_instab_v3, .direction=c("updown")) %>%
  ungroup() %>%
  mutate(child_res_instab_v2 = ifelse(is.na(child_res_instab_v2),0,child_res_instab_v2),
         child_res_instab_v3 = ifelse(is.na(child_res_instab_v3),0,child_res_instab_v3)) %>%
  distinct(hhidpn, .keep_all = TRUE) %>%
  select(c(hhidpn, child_res_instab, child_res_instab_v2, child_res_instab_v3))

table(childhood_instab$child_res_instab, exclude=NULL)
table(childhood_instab$child_res_instab_v3, exclude=NULL)
table(childhood_instab$child_res_instab_v2, exclude=NULL)

# Merge exposure data with HRS rand/trkr/resid data --------------

lhms_resid_cc_first <- lhms_hrs_cc_all %>%
  group_by(hhidpn) %>%
  mutate(response_v2 = row_number()) %>%
  filter(response_v2==1) %>%
  ungroup() %>%
  left_join(childhood_instab, by=c("hhidpn")) %>%
  mutate(across(c(contains("child_res_")), ~ ifelse(is.na(.x), 0, .x))) %>%
  select(-c(rabyear))

# lhms_resid_first <- lhms_resid_fill_first %>%
#   full_join(lhms_resid_cc_first, by=c("hhidpn"))

table(lhms_resid_cc_first$childhood_moves_v2, exclude=NULL)
table(lhms_resid_cc_first$child_res_instab, exclude=NULL)
table(lhms_resid_cc_first$child_res_instab_v3, exclude=NULL)
table(lhms_resid_cc_first$child_res_instab_v2, exclude=NULL)

# Merge exposure data -------------------------------------------

vars <- c("family_financ", "child_forced_move", "father_unemp")

hrs_lhms_exposures <- lhms_hrs_long %>%
  select(-c(starts_with("lh5"))) %>%
  left_join(lhms_resid_cc_first, by=c("hhidpn")) %>%
  group_by(hhidpn) %>%
  # move DK/RF responses to NA for first and last SES vars
  mutate(across(c(any_of(vars)), ~ ifelse(.x %in% c(8,9),NA, .x))) %>%
  mutate(father_occup = ifelse(father_occup %in% c(98, 99), NA, father_occup)) %>%
  mutate(poor_family_financ = case_when(family_financ == 5 ~ 1, 
                                        family_financ %in% c(1,3,6) ~ 0)) %>%
  ungroup()

# table(hrs_lhms_exposures$child_forced_move, hrs_lhms_exposures$hrswave, exclude=NULL)
# table(hrs_lhms_exposures$family_financ, hrs_lhms_exposures$hrswave, exclude=NULL)
# table(hrs_lhms_exposures$father_unemp, hrs_lhms_exposures$hrswave, exclude=NULL)
# table(hrs_lhms_exposures$father_occup, hrs_lhms_exposures$hrswave, exclude=NULL)
# table(hrs_lhms_exposures$family_financ, hrs_lhms_exposures$poor_family_financ, exclude=NULL)

hrs_lhms_exposures_clean <- hrs_lhms_exposures %>%
  group_by(hhidpn) %>%
  mutate(family_financ_count = sum(!is.na(family_financ)),
         child_forced_move_count = sum(!is.na(child_forced_move))) %>%
  mutate(family_financ_chg =  ifelse(n_distinct(family_financ[!is.na(family_financ)]) > 1,
                                     1, 0),
         poor_family_financ_chg =  ifelse(n_distinct(poor_family_financ[!is.na(poor_family_financ)]) > 1,
                                     1, 0),
         child_forced_move_chg =  ifelse(n_distinct(child_forced_move[!is.na(child_forced_move)]) > 1,
                                         1, 0),) %>%
  mutate(first_family_financ = first(na.omit(family_financ)),
         first_family_financ_yr = first(hrswave[!is.na(family_financ)]),
         first_family_financ_age = first(agewave[!is.na(family_financ)]),
         last_family_financ = last(na.omit(family_financ)),
         last_family_financ_age = last(agewave[!is.na(family_financ)]),
         first_forced_move = first(na.omit(child_forced_move)),
         first_forced_move_yr = first(hrswave[!is.na(child_forced_move)]),
         first_forced_move_age = first(agewave[!is.na(child_forced_move)]),
         last_forced_move = last(na.omit(child_forced_move)),
         last_forced_move_age = last(agewave[!is.na(child_forced_move)]),
         first_father_occup = first(na.omit(father_occup)),
         first_father_occup_yr = first(hrswave[!is.na(father_occup)]),
         last_father_occup = last(na.omit(father_occup)),
         first_father_unemp = first(na.omit(father_unemp)),
         first_father_unemp_yr = first(hrswave[!is.na(father_unemp)]),
         last_father_unemp = last(na.omit(father_unemp)),
         lh4e = case_when(lh4e==1 ~ 1,
                          lh4e==5 ~ 0,
                          is.na(lh4e) ~ NA)) %>%
  # not asked 2008!
  mutate(first_father_military = 
           ifelse(first_father_occup==17 & first_father_occup_yr <= 2004, 1,
                  ifelse(first_father_occup==25 & first_father_occup_yr >= 2004 & 
                           first_father_occup_yr <= 2010, 1,
                         ifelse(first_father_occup==23 & first_father_occup_yr >= 2012, 1, 0)))) %>% 
  ungroup()

# Should get 45232 people total, 14,909 people in LHMS sample
length(unique(hrs_lhms_exposures_clean$hhidpn)) 
hrs_lhms_exposures_clean %>% filter(!is.na(lhms_flag)) %>% distinct(hhidpn) %>% nrow()

table(hrs_lhms_exposures_clean$childhood_moves_v2, hrs_lhms_exposures_clean$response_v2, 
      exclude=NULL)

test <- hrs_lhms_exposures_clean %>% filter(response_v2==1)
length(unique(test$hhidpn))

table(hrs_lhms_exposures_clean$first_family_financ, 
      hrs_lhms_exposures_clean$last_family_financ, exclude=NULL) # 8207 missing obs
table(hrs_lhms_exposures_clean$first_forced_move, hrs_lhms_exposures_clean$last_forced_move,
      exclude=NULL) # 9427 missing obs
table(hrs_lhms_exposures_clean$first_father_occup, 
      hrs_lhms_exposures_clean$first_father_occup_yr, exclude=NULL) # 222466 missing obs
table(hrs_lhms_exposures_clean$first_father_unemp, exclude=NULL) # 12202 missing obs
table(hrs_lhms_exposures_clean$lh4e, exclude = NULL) # 154653 missing obs

# Check change in answers 
t <- hrs_lhms_exposures_clean %>% distinct(hhidpn, .keep_all = TRUE)

table(t$child_forced_move_count,
      t$child_forced_move_chg, exclude=NULL)
table(t$family_financ_count,
      t$family_financ_chg, exclude=NULL)
table(t$family_financ_count,
      t$poor_family_financ_chg, exclude=NULL)

# age at answers
summary(t$first_family_financ_age)
summary(t$first_forced_move_age)

summary(t$last_family_financ_age)
summary(t$last_forced_move_age)

t %>% filter(!is.na(first_family_financ_age) &
                                      first_family_financ_age < 50) %>%
  nrow()

t %>% filter(!is.na(last_family_financ_age) &
                                      last_family_financ_age < 50) %>% 
  nrow()

# Save ------------------------------------------------------

## save long CC mobility data and exposure + EMM data set
saveRDS(lhms_hrs_cc_all, here("data", "processed", "lhms_hrs_cc_all.rds"))
saveRDS(hrs_lhms_exposures_clean, here("data", "processed", "hrs_lhms_exposures.rds"))
