# Purpose: Carryforward imputation
# Author: Taylor M. Mobley
# Date: May 10, 2025

library(here)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(haven)
library(mice)
library(openxlsx)
library(VIM)
library(visdat)
library(naniar)

# Read -----
preimp <- readRDS(here("Aim_2", "data", "processed", "dat_fnlim_analysis.rds"))

time_vars <- c("mstat", "itot", "hhres", "shlt", "heart", "diab", "hibp", "strok")

# Hot deck imputations for time-invariant variables -----

## baseline data -----
dat_bl <- preimp %>% 
  group_by(hhidpn) %>%
  filter(hrswave == min(hrswave)) %>%
  ungroup() %>%
  mutate(agecat = findInterval(agey_e, quantile(agey_e, seq(0, 1, 0.25)), 
                               rightmost.closed = TRUE))

# table(dat_bl$agecat, exclude=NULL)
# table(dat_bl$child_forced_move, exclude=NULL) # 343
# table(dat_bl$family_financ, exclude=NULL) # 170
# table(dat_bl$sbirth, exclude=NULL) # 21

# Hot deck imputations ------

set.seed(12345)

dat_hotdeck <- hotdeck(dat_bl, 
                       domain_var = c("agecat", "female", "rabcohort"),
                       variable = c("sbirth", "child_forced_move", 
                                    "family_financ", "raeduc", time_vars),
                       impNA = TRUE)

table(dat_bl$sbirth, dat_hotdeck$sbirth, exclude=NULL)
table(dat_bl$child_forced_move, dat_hotdeck$child_forced_move, exclude=NULL)
table(dat_bl$family_financ, dat_hotdeck$family_financ, exclude=NULL)
table(dat_bl$raeduc, dat_hotdeck$raeduc, exclude=NULL)

# Add imputed data variables to long data set -----

dat_timevar <- preimp %>%
  select(-c("sbirth", "child_forced_move", "family_financ", "raeduc")) %>%
  group_by(hhidpn) %>%
  filter(hrswave != min(hrswave)) %>%
  ungroup() %>%
  bind_rows(dat_hotdeck) %>%
  arrange(hhidpn, hrswave) %>%
  group_by(hhidpn) %>%
  fill(sbirth, child_forced_move, family_financ, raeduc, 
       .direction=c("down")) %>%
  ungroup()

gg_miss_var(dat_timevar %>% 
              select(all_of(c("female", "raceth", "rabcohort", "raeduc",
                       "sbirth", "child_forced_move", "family_financ", 
                        time_vars))), show_pct = TRUE)

# Carryforward time-varying variables -----

dat_timevar <- dat_timevar %>%
  arrange(hhidpn, hrswave) %>%
  group_by(hhidpn) %>%
  fill(all_of(time_vars), .direction = c("down")) %>%
  ungroup()

check <- dat_timevar %>% select("female", "raceth", "rabcohort", "raeduc",
                                "sbirth", "child_forced_move", "family_financ", 
                                all_of(time_vars))

gg_miss_var(check, show_pct = TRUE)

# Save imputed data -----

saveRDS(dat_timevar, file=here("Aim_2", "data", "processed", "hrs_fnlim_imputed.rds"))
