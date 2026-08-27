# Purpose: KM analyses, age as timescale
# Author: Taylor M. Mobley
# Date: May 11, 2025

library(here)
library(survminer)
library(rio)
library(lubridate)
library(magrittr)
library(WeightIt)
library(splines)
library(slider)

source(here("Aim_1/code/0a_libraries.R"))
source(here("Aim_1/code/0d_KM_functions_clean.R"))
options(scipen=999)

# Load -----
dat <- readRDS(here("Aim_1", "data", "processed", "dat_surv_wgts.rds"))

## subset datasets -----
names(dat)
dat_km <- list("dat_surv" = dat[["dat_surv"]],
               "dat_surv_insec" = dat[["dat_surv_insec"]],
               "dat_surv_sec" = dat[["dat_surv_sec"]],
               "dat_surv_5060" = dat[["dat_surv_5060"]],
               "dat_surv_insec_5060" = dat[["dat_surv_insec_5060"]],
               "dat_surv_sec_5060" = dat[["dat_surv_sec_5060"]],
               "dat_surv_over60" = dat[["dat_surv_over60"]],
               "dat_surv_insec_over60" = dat[["dat_surv_insec_over60"]],
               "dat_surv_sec_over60" = dat[["dat_surv_sec_over60"]])

# KM risk bootstraps ------

## set up specs
exp <- c("child_forced_move")
surv_ts <- list("ages" = seq(55, 90, by=1),
                "ages5060" = seq(55, 80, by=1),
                "agesover60" = seq(65, 90, by=1))
ends <- tibble(end = c("age_end2"),
               type = c(2))

forms <- tibble("demo_iptw" = "child_forced_move ~ female + raceth + factor(rabcohort)",
                "all_iptw" = "child_forced_move ~ female + raceth + feduc_le8 + 
                meduc_le8 + sbirth + factor(rabcohort)")

## set up number of bootstraps
bootn <- 500

if (exists("KM_results")) rm(KM_results)

for(m in 1:length(dat_km)){
  
  # print(paste0("m = ", m))
  
  if(str_detect(names(dat_km[m]), "5060")){
    b = 2
    } else if(str_detect(names(dat_km[m]), "over60")){
    b = 3
    } else{
    b = 1
    }
  
  # print(paste0("b = ", b))
  
  for(p in unique(exp)){
    
    # print(paste0("p = ", p))
    
    for(q in 1:nrow(ends)){
    
      # print(paste0("q = ", q))
      
    end <- ends[q, ]$end

    if(names(surv_ts[b]) %in% c("ages", "ages5060", "agesover60")){
        v <- c("NULL", "demo_iptw", "all_iptw")
      } else{
        v <- c("NULL", "demo_agebl_iptw", "all_agebl_iptw")
      }
    
      for(c in unique(v)){
        
        # print(paste0("c = ", c))
        
        ## run in full sample to get point estimates, ns
        w <- dat_km[[m]][[c]]
        
        full <- survfit(as.formula(paste("Surv(age_start,", end, ", death_flag) ~ ", exp)),
                        data=dat_km[[m]], weights = w) 
        
          ## get estimates, nrisk at 5-year intervals
          summary_tab <- full %>%
            summary(, times = surv_ts[[b]])
          
          ## get running cumulative events
          ref_years <- tibble(target_year = seq(
            from = min(surv_ts[[b]]) - 5,
            to   = max(surv_ts[[b]]),
            by   = 1))
          
          cum_events <- tidy(full) %>%
            group_by(strata) %>%
            arrange(time) %>%
            cross_join(ref_years) %>%
            filter((time > (target_year - 1) & time <= target_year)) %>%
            mutate(cumevents_tot = sum(n.event)) %>%
            group_by(strata, target_year) %>%
            mutate(cumevents = sum(n.event)) %>%
            ungroup() %>%
            rename(exp = strata,
                   age = target_year) %>%
            distinct(exp, age, .keep_all=TRUE) %>%
            group_by(exp) %>%
            mutate(
              cumevents_5yr = if_else(
                age %% 5 == 0,
                slide_dbl(cumevents, sum, .before = 4, .complete = FALSE),
                NA_real_
              )
            ) %>%
            ungroup() %>%
            select(exp, age, starts_with("cumevents")) 
          
          summary_tab <- data.frame(sample = names(dat_km[m]),
                                    nrisk = summary_tab$n.risk,
                                    nevent = summary_tab$n.event,
                                    age = summary_tab$time, # to merge with the output below
                                    exp = summary_tab$strata,
                                    estimate = 1 - summary_tab$surv) %>%
            left_join(cum_events, by=c("exp", "age")) %>%
            fill(cumevents, cumevents_tot, cumevents_5yr, .direction=c("up")) %>%
            mutate(exp = case_when(exp == "child_forced_move=1" ~ 1,
                                   exp == "child_forced_move=0" ~ 0)) %>%
            pivot_wider(
              names_from = exp,
              values_from = c(nrisk, nevent, cumevents, cumevents_tot,
                              cumevents_5yr, estimate),
              names_glue = "{.value}_{exp}"
              
            ) %>%
            mutate(
              risk_ratio = estimate_1 / estimate_0,
              risk_difference = (estimate_1 - estimate_0)*100
            )
       
        ## run bootstrap
        temp <- risks_boots(dat_km[[m]], bootn, 12345, p, end,
                           c, surv_ts[[b]], forms[[c]]) %>%
          select(age, ends_with("_LL"), ends_with("_UL")) %>%
          mutate(sample = names(dat_km[m]), 
                 exposure = p, 
                 endtime = ends[q, ]$type,
                 timescale = names(surv_ts[b]),
                 weight = c) %>%
          ## join results from full sample
          left_join(summary_tab, by=c("sample", "age"))
        
        if(!exists("KM_results")){
          assign("KM_results", temp) 
        } else{
          KM_results <- plyr::rbind.fill(KM_results, temp)
        }
      }
    }
  }
}

# 6 warnings 3/5/2026 -- I spoke to Juliet about this on 8/6/2026.
# We looked through the loop step by step and think these warnings are coming 
# from std error calculations in the bootstrapped sample survfits. This 
# shouldn't impact our results because we don't use the bootstrapped SEs

# Round ages for plotting, etc.

KM_results <- KM_results %>%
  mutate(age = round(age,0),
         boots = bootn) 

test <- KM_results %>% filter(sample=="dat_surv" & endtime==2) %>% 
  arrange(age, weight)

# Save -----

saveRDS(KM_results, here("Aim_1", "data", "processed", "KM_results_age.rds"))
