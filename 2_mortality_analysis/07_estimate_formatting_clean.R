# Purpose: 
# Author: Taylor M. Mobley
# Date: Feb 20, 2026

library(here)
library(survival)
library(survminer)
library(rio)
library(lubridate)
library(tidyverse)
library(magrittr)
library(survival)
library(WeightIt)
library(cobalt)
library(splines)

# Load -----

dat_surv <- readRDS(here("Aim_1", "data", "processed", "dat_surv_wgts.rds"))
KM_results_age <- readRDS(here("Aim_1", "data", "processed", "KM_results_age.rds"))
KM_results_time <- readRDS(here("Aim_1", "data", "processed", "KM_results_time.rds"))
KM_results_sens <- readRDS(here("Aim_1", "data", "processed", "KM_results_sens.rds"))

# Format everything except supp (do next) -----

KM_results_f <- KM_results_age %>%
  rbind(KM_results_time) %>%
  rbind(KM_results_sens) %>%
  ungroup() %>%
  mutate(sample_f = factor(case_when(sample == "dat_surv" ~ paste0("Overall,\n(n = ", nrow(dat_surv[["dat_surv"]]), ")"),
                                     sample == "dat_surv_insec" ~ paste0("Childhood financial insecurity,\n(n = ", nrow(dat_surv[["dat_surv_insec"]]), ")"),
                                     sample == "dat_surv_sec" ~ paste0("No childhood financial insecurity,\n(n = ", nrow(dat_surv[["dat_surv_sec"]]), ")"),
                                     sample == "dat_surv_cc" ~ paste0("Complete case, overall\n(n = ", nrow(dat_surv[["dat_surv_cc"]]), ")"),
                                     sample == "dat_surv_insec2" ~ paste0("Childhood financial insecurity (v2),\n(n = ", nrow(dat_surv[["dat_surv_insec2"]]), ")"),
                                     sample == "dat_surv_sec2" ~ paste0("No childhood financial insecurity (v2),\n(n = ", nrow(dat_surv[["dat_surv_sec2"]]), ")"),
                                     sample == "dat_surv_5060" ~ paste0("Overall, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_5060"]]), ")"),
                                     sample == "dat_surv_over60" ~ paste0("Overall, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_over60"]]), ")"),
                                     sample == "dat_surv_insec_5060" ~ paste0("Childhood financial insecurity, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_insec_5060"]]), ")"),
                                     sample == "dat_surv_insec_over60" ~ paste0("Childhood financial insecurity, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_insec_over60"]]), ")"),
                                     sample == "dat_surv_sec_5060" ~ paste0("No childhood financial insecurity, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_sec_5060"]]), ")"),
                                     sample == "dat_surv_sec_over60" ~ paste0("No childhood financial insecurity, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_sec_over60"]]), ")"),
                                     sample == "dat_surv_cc_5060" ~ paste0("Complete case, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_cc_5060"]]), ")"),
                                     sample == "dat_surv_insec2_5060" ~ paste0("Childhood financial insecurity (v2), baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_insec2_5060"]]), ")"),
                                     sample == "dat_surv_sec2_5060" ~ paste0("No childhood financial insecurity (v2), baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_sec2_5060"]]), ")"),
                                     sample == "dat_surv_cc_over60" ~ paste0("Complete case, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_cc_over60"]]), ")"),
                                     sample == "dat_surv_insec2_over60" ~ paste0("Childhood financial insecurity (v2), baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_insec2_over60"]]), ")"),
                                     sample == "dat_surv_sec2_over60" ~ paste0("No childhood financial insecurity (v2), baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_sec2_over60"]]), ")")),
                           levels = c(paste0("Overall,\n(n = ", nrow(dat_surv[["dat_surv"]]), ")"), 
                                      paste0("Childhood financial insecurity,\n(n = ", nrow(dat_surv[["dat_surv_insec"]]), ")"), 
                                      paste0("No childhood financial insecurity,\n(n = ", nrow(dat_surv[["dat_surv_sec"]]), ")"),
                                      paste0("Overall, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_5060"]]), ")"),
                                      paste0("Overall, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_over60"]]), ")"),
                                      paste0("Childhood financial insecurity, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_insec_5060"]]), ")"),
                                      paste0("Childhood financial insecurity, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_insec_over60"]]), ")"),
                                      paste0("No childhood financial insecurity, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_sec_5060"]]), ")"),
                                      paste0("No childhood financial insecurity, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_sec_over60"]]), ")"),
                                      paste0("Childhood financial insecurity (v2),\n(n = ", nrow(dat_surv[["dat_surv_insec2"]]), ")"), 
                                      paste0("No childhood financial insecurity (v2),\n(n = ", nrow(dat_surv[["dat_surv_sec2"]]), ")"),
                                      paste0("Complete case, overall\n(n = ", nrow(dat_surv[["dat_surv_cc"]]), ")"),
                                      paste0("Complete case, baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_cc_5060"]]), ")"),
                                      paste0("Childhood financial insecurity (v2), baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_insec2_5060"]]), ")"),
                                      paste0("No childhood financial insecurity (v2), baseline age 50-60 years\n(n = ", nrow(dat_surv[["dat_surv_sec2_5060"]]), ")"),
                                      paste0("Complete case, baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_cc_over60"]]), ")"),
                                      paste0("Childhood financial insecurity (v2), baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_insec2_over60"]]), ")"),
                                      paste0("No childhood financial insecurity (v2), baseline age >60 years\n(n = ", nrow(dat_surv[["dat_surv_sec2_over60"]]), ")")))
  ) %>%
  mutate(weight_f = factor(case_when(weight == "NULL" ~ "Unweighted",
                                     weight == "demo_iptw" ~ "Demographic weights",
                                     weight == "all_iptw" ~ "Fully adjusted weights",
                                     weight == "demo_agebl_iptw" ~ "Demographic weights",
                                     weight == "all_agebl_iptw" ~ "Fully adjusted weights"),
                           levels = c("Unweighted",
                                      "Demographic weights",
                                      "Fully adjusted weights"))) %>%
  # mutate(across(c(estimate_1, estimate_0), ~ .x*100)) %>%
  mutate(across(c(nevent_0, nevent_1, nrisk_0, nrisk_1, starts_with("cumevent")), ~ round(.x, 0)))
  # mutate(across(c(starts_with("estimate")), ~ round(.x, 3)))

## checks
table(KM_results_f$sample, KM_results_f$sample_f, exclude=NULL)
test <- KM_results_f %>% filter(sample=="dat_surv" & age <= 25 & endtime==2) %>% arrange(age, weight)

# Save -----

saveRDS(KM_results_f, here("Aim_1", "data", "processed", "KM_results_f.rds"))
