# Purpose: Format means + contrast estimates
# Author: Taylor M. Mobley
# Date: May 16, 2025

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)
library(lme4)
library(emmeans)
library(stringr)
library(splines)
library(grid)
library(egg)

# Import --------------------------------------------------

preds_time <- readRDS(here("Aim_2", "data", "processed", "preds_time.rds"))
preds_adl <- readRDS(here("Aim_2", "data", "processed", "preds_adls.rds"))

contrasts_time <- readRDS(here("Aim_2", "data", "processed", "contrasts_time.rds"))
contrasts_adl <- readRDS(here("Aim_2", "data", "processed", "contrasts_adls.rds"))

# Merge preds and contrasts

## merge 
preds <- bind_rows(preds_time %>% mutate(outcome = "fnlim"), 
                   preds_adl %>% mutate(outcome = "adl6a"))
contrasts <- bind_rows(contrasts_time %>% mutate(outcome = "fnlim"), 
                       contrasts_adl %>% mutate(outcome = "adl6a"))

colnames(preds)

# Clean preds ------

preds_clean <- preds %>%
  mutate(time = time_start10*10,
         # age = (agewave_c10*10) + 65,
         age_bl = (age_bl_c10*10) + 65) %>%
  mutate(child_forced_move = factor(case_when(child_forced_move==1 ~ "Childhood housing instability",
                                       child_forced_move==0 ~ "No childhood housing instability"),
                                    levels=c("Childhood housing instability",
                                             "No childhood housing instability"))) %>%
  mutate(sample_f = factor(case_when(sample=="dat_fnlim_all" ~ "Overall",
                                     sample=="dat_fnlim_insec" ~ "Childhood financial insecurity",
                                     sample=="dat_fnlim_sec" ~ "No childhood financial insecurity"),
                                     # sample=="dat_fnlim_all_5060" ~ "Overall",
                                     # sample=="dat_fnlim_insec_5060" ~ "Childhood financial insecurity",
                                     # sample=="dat_fnlim_sec_5060" ~ "No childhood financial insecurity",
                                     # sample=="dat_fnlim_all_over60" ~ "Overall",
                                     # sample=="dat_fnlim_insec_over60" ~ "Childhood financial insecurity",
                                     # sample=="dat_fnlim_sec_over60" ~ "No childhood financial insecurity"),
                           levels = c("Overall",
                                      "Childhood financial insecurity",
                                      "No childhood financial insecurity"))) %>%
  mutate(weight_f = factor(case_when(weight=="" ~ "Crude",
                                     weight=="demo_iptw" ~ "As observed (demo IPTW)",
                                     weight=="all_iptw" ~ "As observed (IPTW)",
                                     weight=="td.weight.time.trunc" ~ "Eliminating death (IPTW*IPDW)",
                                     weight=="tc.weight.time" ~ "Eliminating LTFU (IPTW*IPCW)",
                                     weight=="all.weight.time" ~ "Eliminating death and LTFU (LTFU not truncated) (IPTW*IPDW*IPCW)",
                                     weight=="all.weight.time.trunc" ~ "Eliminating death and LTFU, truncated (IPTW*IPDW*IPCW)",
                                     weight=="comb.weight.time.trunc" ~ "Eliminating death and LTFU combined, truncated (IPTW*IPCDW)"))) %>%
  mutate(emmean = ifelse(model=="pr", exp(emmean), emmean)) %>%
  mutate(lower.CL = ifelse(model=="pr", exp(lower.CL), lower.CL)) %>%
  mutate(upper.CL = ifelse(model=="pr", exp(upper.CL), upper.CL)) %>%
  mutate(age_bl_f = case_when(age_bl==50 ~ "Baseline age 50 years",
                              age_bl==60 ~ "Baseline age 60 years",
                              age_bl==70 ~ "Baseline age 70 years"))

table(preds_clean$sample_f, exclude=NULL)

# Clean contrasts -----

contrasts_clean <- contrasts %>%
  mutate(time = time_start10*10) %>%
  # mutate(age = (agewave_c10*10)+65) %>%
  mutate(age_bl = age_bl_c10*10 + 65) %>%
  mutate(sample_f = factor(case_when(sample=="dat_fnlim_all" ~ "Overall",
                                     sample=="dat_fnlim_insec" ~ "Childhood financial insecurity",
                                     sample=="dat_fnlim_sec" ~ "No childhood financial insecurity"),
                                     # sample=="dat_fnlim_all_5060" ~ "Overall",
                                     # sample=="dat_fnlim_insec_5060" ~ "Childhood financial insecurity",
                                     # sample=="dat_fnlim_sec_5060" ~ "No childhood financial insecurity",
                                     # sample=="dat_fnlim_all_over60" ~ "Overall",
                                     # sample=="dat_fnlim_insec_over60" ~ "Childhood financial insecurity",
                                     # sample=="dat_fnlim_sec_over60" ~ "No childhood financial insecurity"),
                           levels = c("Overall",
                                      "Childhood financial insecurity",
                                      "No childhood financial insecurity"))) %>%
  mutate(weight_f = factor(case_when(weight=="" ~ "Crude",
                                     weight=="demo_iptw" ~ "As observed (demo IPTW)",
                                     weight=="all_iptw" ~ "As observed (IPTW)",
                                     weight=="td.weight.time.trunc" ~ "Eliminating death (IPTW*IPDW)",
                                     weight=="tc.weight.time" ~ "Eliminating LTFU (IPTW*IPCW)",
                                     weight=="all.weight.time" ~ "Eliminating death and LTFU (LTFU not truncated) (IPTW*IPDW*IPCW)",
                                     weight=="all.weight.time.trunc" ~ "Eliminating death and LTFU, truncated (IPTW*IPDW*IPCW)",
                                     weight=="comb.weight.time.trunc" ~ "Eliminating death and LTFU combined, truncated (IPTW*IPCDW)"))) %>%
  mutate(LL = estimate - 1.96*SE,
         UL = estimate + 1.96*SE) %>%
  mutate(estimate = ifelse(model=="pr", exp(estimate), estimate),
         LL = ifelse(model=="pr", exp(LL), LL),
         UL = ifelse(model=="pr", exp(UL), UL)) %>%
  mutate(age_bl_f = case_when(age_bl==50 ~ "Baseline age 50 years",
                              age_bl==60 ~ "Baseline age 60 years",
                              age_bl==70 ~ "Baseline age 70 years"))

contrasts_clean <- contrasts_clean %>%
  mutate(contrast_f = "Childhood housing instability") %>%
  mutate(age_bl_f = case_when(age_bl==50 ~ "Baseline age 50 years",
                              age_bl==60 ~ "Baseline age 60 years",
                              age_bl==70 ~ "Baseline age 70 years")) 

table(contrasts_clean$contrast_f, exclude=NULL)
table(contrasts_clean$sample_f, exclude=NULL)

# Save -----

saveRDS(preds_clean, here("Aim_2", "data", "processed", "preds_clean.rds"))
saveRDS(contrasts_clean, here("Aim_2", "data", "processed", "contrasts_clean.rds"))
