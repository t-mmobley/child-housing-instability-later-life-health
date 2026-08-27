# Purpose: Hot deck imputations
# Author: Taylor M. Mobley
# Date: Feb 2, 2026

library(here)
source(here("Aim_1/code/0a_libraries.R"))

# Read -----
dat <- readRDS(here("Aim_1", "data", "processed", "dat_surv.rds"))

vars <- c("age_start", "age_end", "female", "raceth", "sbirth", 
          "child_forced_move", "family_financ", "rabcohort", "rameduc",
          "rafeduc")

# check out missingness ------

preimp <- dat %>% select(any_of(c("hhidpn", vars)))

p <- gg_miss_var(preimp, show_pct = TRUE)
p

ggsave(
  filename = here("Aim_1", "results", "missingness.png"),
  plot = p,
  dpi = 300
)

# Hot deck imputations for time-invariant variables -----

## baseline data -----
preimp <- preimp %>% 
  # make missing category
  mutate(raceth = ifelse(is.na(raceth), "NA", raceth)) %>%
  mutate(agecat = findInterval(age_start, quantile(age_start, seq(0, 1, 0.25)), 
                               rightmost.closed = TRUE))

table(preimp$agecat, exclude=NULL)
table(preimp$raceth, exclude=NULL)
table(preimp$rabcohort, exclude=NULL)
table(preimp$child_forced_move, exclude=NULL) # 328
table(preimp$family_financ, exclude=NULL) # 154
table(preimp$sbirth, exclude=NULL) # 28

# Hot deck imputations ------

set.seed(12345)

dat_hotdeck <- hotdeck(preimp, 
                       domain_var = c("agecat", "female", "rabcohort"),
                       variable = c("sbirth", "child_forced_move", 
                                    "family_financ"),
                       impNA = TRUE)

table(preimp$sbirth, dat_hotdeck$sbirth, exclude=NULL)
table(preimp$child_forced_move, dat_hotdeck$child_forced_move, exclude=NULL)
table(preimp$family_financ, dat_hotdeck$family_financ, exclude=NULL)

# Add imputed data variables to long data set -----

dat_imp <- dat %>%
  select(-c("sbirth", "child_forced_move", "family_financ", "raceth")) %>%
  bind_cols(dat_hotdeck %>% 
              select(all_of(c("raceth", "sbirth", 
                              "child_forced_move", "family_financ")))) %>%
  # Extra data cleaning -----
  # re-create poor family financ
  mutate(poor_family_financ = ifelse(family_financ == "Poor", 1, 0)) %>%
  # re-create poor/varied family financ
  mutate(poorvar_family_financ = ifelse(family_financ %in% c("Poor", "Varied"), 1, 0)) %>%
  # recreate 4-level forced move + mobility variable
  mutate(child_move_insec4 = case_when(child_forced_move==1 & high_childhood_moves_v2==1 ~ 4,
                                       child_forced_move==1 & high_childhood_moves_v2==0 ~ 3,
                                       child_forced_move==0 & high_childhood_moves_v2==1 ~ 2,
                                       child_forced_move==0 & high_childhood_moves_v2==0 ~ 1)) %>%
  # create 2-level exposure from forced move + mobility vars
  mutate(child_move_insec2 = case_when(child_forced_move==1 & high_childhood_moves_v2==1 ~ 1,
                                       child_forced_move==1 & high_childhood_moves_v2==0 ~ 0,
                                       child_forced_move==0 & high_childhood_moves_v2==1 ~ 0,
                                       child_forced_move==0 & high_childhood_moves_v2==0 ~ 0))

gg_miss_var(dat_imp %>% select(all_of(c(vars)), "poor_family_financ", 
                               "poorvar_family_financ", "feduc_le8",
                               "meduc_le8", "feduc3", "meduc3"), show_pct = TRUE)

# Save imputed data -----

saveRDS(dat_imp, file=here("Aim_1", "data", "processed", "dat_surv_imp.rds"))
