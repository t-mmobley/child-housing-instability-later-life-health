# Purpose: Create analytic sample for functional limitations
# Author: Taylor M. Mobley
# Date: Nov 1, 2024

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)
library(gtsummary)
library(flextable)

# Import ---------------------------------------------------
dat <- readRDS(here("data", "processed", "hrs_lhms_clean.rds"))

names(dat)
length(unique(dat$hhidpn)) # 38042

# Select variables of interest -----

vars <- c("hhidpn", "hrswave", "agewave", "female", "raceth", "sbirth", 
          "rabyear", "rabcohort", "feduc_le8", "meduc_le8", "child_forced_move", 
          "raeduc", "mstat", "itot", "hhres", "family_financ", "shlt", "cesd",
          "adl6a", "iadl5a", "heart", "diab", "hibp", "strok", "raddate", 
          "rabdate", "death_flag", "eos", "iwend", "iwbeg", "lastaliveyear",
          "lastalivemonth", "agey_e",  "agey_b", "agey_m", "age_lhms")

descr_vars <- c("father_unemp", "first_father_military", "lhms_flag", 
                "lhms_year", "lh4e", "high_childhood_moves_v2", "ravetrn", 
                "radage_y", "radage", "cesdm", "knowndeceasedsource", "radyear",
                "child_forced_move_count", "child_forced_move_chg",
                "family_financ_count", "family_financ_chg",
                "poor_family_financ_chg", "rafeduc", "rameduc")

nontime_vars <- c("hhidpn", "female", "raceth", "sbirth", "rabcohort",
                  "feduc_lt8", "meduc_lt8", "child_forced_move", 
                  "raeduc",  "family_financ", "death_flag", "eos",
                  "father_unemp", "first_father_military", 
                  "lh4e", "high_childhood_moves_v2", "ravetrn", "age_hlms",
                  "death_age", "radyear", "knowndeceasedsource")

time_vars <- c("hrswave", "agey_e", "mstat", "itot", "hhres", "shlt",  "cesd",
               "adl6a", "iadl5a", "heart", "diab", "hibp", "strok")

dat_vars <- dat %>% select(any_of(c(vars, descr_vars)))

# Filter no missing I/ADLs -----

dat_fnlim_preMI <- dat_vars %>% filter(!is.na(adl6a) & !is.na(iadl5a))
length(unique(dat_fnlim_preMI$hhidpn)) # 37986

# Check health condition categories for missingness -----

# RAND 1992-2022 longitudinal file documentation
# "RwHIBP, RwDIAB, RwCANCR, RwLUNG, RwHEART, RwSTROK, RwPSYCH, and RwARTHR are recoded to reflect the
# answer given. Note that there is not an option to dispute a prior report of RwSLEEP, and therefore, no values are recoded. The meaning
# of the answer depends on the question asked, so the analyst may want to consider these variables in conjunction with the corresponding
# "Q" variables. Generally, a "1" means "yes" and "0" means "no." Note that for a re-interview, when the condition was previously
# reported, a "1" in this variable simply means the Respondent did not dispute the statement about the records from last interview. In
# Waves 3 and 4, this can be disputed, but no follow-up as to whether the Respondent currently has the condition is done; these are
# assigned a code of "5." From Waves 5 through 13, the interviewer is asked to probe for current condition if the statement is disputed;
# these are assigned a code of "3" or "4," depending on whether the Respondent says they have the condition now or not, respectively.
# From Wave 14 forward and with the exception of RwSLEEP, the interviewer can retroactively assign "hidden" values of "4. Never had
# [condition]" or "6. Had [condition] before, but do not have it now..." if in the subsequent question the Respondent disputes a prior report
# of having had the condition.

# R4HEART is set to "6" if the raw variable indicates that the incorrect question is asked due to a preload problem, and the Respondent 
# reports no condition since last interview. These cases had previously reported a heart condition."

# In general:
# 1 = yes
# 0 = no
# 2 = TIA/possible stroke
# 3 = previously did not have, now has
# 4 = previously had, now does not have
# 5 = from waves 3 and 4 if previously had and now disputes but HRS didn't probe whether now have or not
# 6 = wave 14 on (except sleep), Had [condition] before, but do not have it now..."

table(dat_fnlim_preMI$heart, dat_fnlim_preMI$hrswave, exclude=NULL)
table(dat_fnlim_preMI$diab, exclude=NULL)
table(dat_fnlim_preMI$hibp, exclude=NULL)
table(dat_fnlim_preMI$strok, exclude=NULL)

health.vars <- c("heart", "diab", "hibp", "strok")

dat_fnlim_preMI <- dat_fnlim_preMI %>%
  mutate(heart = ifelse(hrswave==1998 & heart==6, 1, heart)) %>%
  mutate(across(all_of(health.vars), ~ case_when(is.na(.x) ~ NA,
                                             .x %in% c(1,2,3) ~ 1,
                                             .x %in% c(0,4,6) ~ 0,
                                             hrswave==1998 & .x==5 ~ NA)))

table(dat_fnlim_preMI$heart, dat_fnlim_preMI$hrswave, exclude=NULL)
table(dat_fnlim_preMI$diab, exclude=NULL)
table(dat_fnlim_preMI$hibp, exclude=NULL)
table(dat_fnlim_preMI$strok, exclude=NULL)
  
# Missingness EDA -------------------------------------

library(naniar)
library(visdat)

## Missingness with CESD -----
datmiss <- dat_fnlim_preMI %>%
  select(-c(first_father_military, high_childhood_moves_v2, knowndeceasedsource,
            radage_y, lh4e, radyear))

# vis_miss(datmiss, warn_large_data = FALSE)
gg_miss_var(datmiss, show_pct = TRUE)
gg_miss_var(datmiss, show_pct = TRUE, facet = hrswave)

## Missingness without CESD -----
datmiss <- dat_fnlim_preMI %>%
  select(-c(first_father_military, high_childhood_moves_v2, knowndeceasedsource,
            radage_y, lh4e, radyear, father_unemp, death_flag,
            eos, ravetrn, cesdm))

colnames(datmiss)

datmiss <- datmiss %>%
  rename("Forced childhood move" = child_forced_move,
         "Age (years)" = agey_e,
         "Sex/gender" = female,
         "Race and ethncity" = raceth,
         "Southern birth" = sbirth,
         "Birth cohort" = rabcohort,
         "Father education" = feduc_le8,
         "Mother education" = meduc_le8,
         "Educational attainment" = raeduc,
         "Marital status" = mstat,
         "Household income" = itot,
         "Number of household residents" = hhres,
         "Childhood financial well-being" = family_financ,
         "Depressive symptoms" = cesd,
         "Self-rated health" = shlt,
         "ADLs" = adl6a,
         "IADLs" = iadl5a,
         "SR heart problems" = heart,
         "SR diabetes" = diab,
         "SR high BP" = hibp,
         "SR stroke" = strok)

datmiss1 <- datmiss %>%
  select(-c("Forced childhood move", "Childhood financial well-being",
            "Sex/gender", "Race and ethncity", "Southern birth",
            "Birth cohort", "Father education", "Mother education",
            "Educational attainment"))

# vis_miss(datmiss, warn_large_data = FALSE)
gg_miss_var(datmiss1, show_pct = TRUE)
# gg_miss_var(datmiss, show_pct = TRUE, facet = hrswave)

datmiss2 <- datmiss %>%
  distinct(hhidpn, .keep_all=TRUE) %>%
  select(c("Forced childhood move", "Childhood financial well-being",
           "Sex/gender", "Race and ethncity", "Southern birth",
           "Birth cohort", "Father education", "Mother education",
           "Educational attainment"))

gg_miss_var(datmiss2, show_pct = TRUE)

# Number of people in each wave
length(unique(datmiss$hhidpn)) # 37986
r <- datmiss %>% group_by(hrswave) %>% count() %>% ungroup()
t <- datmiss %>% group_by(hrswave, `Forced childhood move`) %>% count() %>%
  ungroup() %>% pivot_wider(names_from = `Forced childhood move`,
                            values_from = c(n)) %>%
  left_join(r, by=c("hrswave")) %>%
  select(c(hrswave, n, everything())) %>%
  rename("No forced childhood move" = `0`,
         "Forced childhood move" = `1`,
         "Missing forced childhood move" = `NA`,
         "Total participants" = `n`) %>%
  flextable()
t

all_years <- seq(1998,2022, by=2)

test <- dat_fnlim_preMI %>%
  select(c(hhidpn, hrswave, radyear)) %>%
  mutate(participate = 1) %>%
  group_by(hhidpn) %>%
  mutate(hrswave_bl = min(hrswave)) %>%
  ungroup() %>%
  complete(hhidpn, hrswave = all_years) %>%
  group_by(hhidpn) %>%
  fill(hrswave_bl, .direction = c("downup")) %>%
  fill(radyear, .direction = c("updown")) %>%
  ungroup() %>%
  mutate(participate = ifelse(is.na(participate), 0, participate),
         notenrolled = ifelse(hrswave < hrswave_bl, 1, 0),
         death = ifelse(hrswave >= radyear, 1, 0),
         ltfu = ifelse(participate !=1 & (death != 1 | is.na(death)), 1, 0),
         status = case_when(participate == 1 ~ "in study",
                            notenrolled == 1 ~ "not enrolled",
                            death == 1 ~ "dead",
                            ltfu == 1 ~ "LTFU"))

table(test$participate, test$death, exclude=NULL)
table(test$status, exclude=NULL)

testwide <- test %>%
  mutate(hrswave = paste0("status_", hrswave)) %>%
  rename("id" = hhidpn) %>%
  pivot_wider(id_cols = id, names_from = "hrswave",
                                 values_from = c(status))

library(ggsankey)
library(ggplot2)

q <- ggsankey::make_long(testwide, starts_with("status")) %>%
  mutate(x = str_replace(x, "status_", ""))

ggplot(q, aes(
  x = x, next_x = next_x,
  node = node, next_node = next_node,
  fill = node
)) +
  geom_sankey(flow.alpha = 0.6, node.color = "gray40") +
  geom_sankey_label(aes(label = node), size = 3, color = "black") +
  theme_sankey(base_size = 14) +
  labs(title = "Participant Transitions Across Years",
       fill = "Group")

# Make cesd missingness a category -----

dat_fnlim_preMI <- dat_fnlim_preMI %>%
  mutate(cesd = ifelse(is.na(cesd), "NA", cesd)) %>%
  select(-c(cesdm))

class(dat_fnlim_preMI$cesd)
table(dat_fnlim_preMI$cesd, exclude=NULL)

# Create race and ethnicity missing category ----
table(dat_fnlim_preMI$female, exclude=NULL) # no sex/gender missingness
nrow(dat_fnlim_preMI %>% filter(is.na(raceth)) %>% distinct(hhidpn)) # 459 people

dat_fnlim_analysis <- dat_fnlim_preMI %>%
  mutate(raceth = as.factor(ifelse(is.na(raceth), 5, raceth)))

table(dat_fnlim_analysis$raceth, exclude=NULL) 
length(unique(dat_fnlim_analysis$hhidpn)) # 37986 

## child_forced_move, family_financ missingness -----
nrow(dat_fnlim_analysis %>% filter(is.na(child_forced_move)) %>% distinct(hhidpn)) # 779 people
nrow(dat_fnlim_analysis %>% filter(is.na(family_financ)) %>% distinct(hhidpn)) # 604 people

# Save -----

## pre-imputation data
saveRDS(dat_fnlim_analysis, here("Aim_2", "data", "processed", "dat_fnlim_analysis.rds"))
