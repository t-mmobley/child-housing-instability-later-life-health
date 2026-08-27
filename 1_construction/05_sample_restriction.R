# Purpose: Create analytic sample
# Author: Taylor M. Mobley
# Date: Nov 1, 2024

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)
library(gtsummary)

# Import ---------------------------------------------------
hrs_lhms_exposures <- readRDS(here("data", "processed", "hrs_lhms_exposures.rds")) 

length(unique(hrs_lhms_exposures$hhidpn)) # 45232 people

# Sample restrictions -----------------------------------

## noting how many people enter in 2022
t <- hrs_lhms_exposures %>% group_by(hhidpn) %>% filter(min(hrswave)==2022) %>% ungroup()
length(unique(t$hhidpn)) # 2829 people

## First, 1998+ waves -----

hrs_lhms1998 <- hrs_lhms_exposures %>% filter(hrswave >= 1998)
length(unique(hrs_lhms1998$hhidpn)) # 41232

## Second, ages 50+ at beginning of interviews -----

hrs_lhms_age <- hrs_lhms1998 %>% group_by(hhidpn) %>%
  filter(min(agey_b) >= 50) %>%
  ungroup()

length(unique(hrs_lhms_age$hhidpn)) # 38042

summary(hrs_lhms_age$agey_e)
summary(hrs_lhms_age$first_forced_move_age)
summary(hrs_lhms_age$first_forced_move_yr)
table(hrs_lhms_age$hrswave, exclude=NULL)

## Check Aim 1 sample -----

### should get 35405
t <- hrs_lhms_age %>% group_by(hhidpn) %>%
  filter(min(hrswave) != 2022) %>%
  ungroup()
length(unique(t$hhidpn)) # 35405

t2 <- hrs_lhms_exposures %>% group_by(hhidpn) %>%
  filter(min(hrswave) != 2022) %>%
  ungroup()
length(unique(t$hhidpn)) # 42403

t2 <- t2 %>% filter(hrswave >= 1998)
length(unique(t$hhidpn)) # 38403

t2 <- t2 %>% group_by(hhidpn) %>%
  filter((hrswave == 1998 & agey_b>=50) | min(agey_b) >= 50) %>%
  ungroup()

length(unique(t2$hhidpn)) # 35405
summary(hrs_lhms_age$agey_e)
summary(hrs_lhms_age$first_forced_move_age)
summary(hrs_lhms_age$first_forced_move_yr)

# Save ------------------------------------------------

saveRDS(hrs_lhms_age, here("data", "processed", "hrs_lhms1998.rds"))
