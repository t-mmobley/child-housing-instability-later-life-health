# Purpose: Continuous score GEE models
# Author: Taylor M. Mobley
# Date: Nov 1, 2024

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(here)
library(haven)
library(geepack)
library(gee)
library(rms)
library(contrast)
library(emmeans)
library(epiR)
library(ggeffects)
library(splines)

options(scipen=999)

# Import ---------------------------------------------------

dats <- readRDS(here("Aim_2", "data", "processed", "dat_wgts.rds"))

dats$dat_lhms_cc <- NULL

## weights -----

wgts <- c("", "demo_iptw", "all_iptw", "td.weight.time.trunc", 
          "tc.weight.time.trunc", "all.weight.time", "all.weight.time.trunc",
          "comb.weight.time.trunc")

forms <- tibble(intx2 = "+",
                intx3 = "*")

# Continuous outcome -----

all_models <- list()
remove(preds)
remove(contrasts)

for(i in 1:length(dats)){
  for(j in unique(wgts)){
    # Note: only doing intx3
    # for(k in 1:length(forms)){
    for(k in 2){
    
    d <- dats[[i]]
    
    # get quantile values for knots
    # quantile(d$time_start10, prob = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
    time_ns <- paste0("ns(time_start10, knots = c(",
                      # round(quantile(d$time_start10, prob = c(0.25)),1), ",",
                      round(quantile(d$time_start10, prob = c(0.50)),1), 
                      # ",",
                      # round(quantile(d$time_start10, prob = c(0.75)),1), 
                      "), Boundary.knots = c(",
                      round(quantile(d$time_start10, prob = c(0.10)),1), ",",
                      round(quantile(d$time_start10, prob = c(0.90)),1),"))")
    
    agebl_ns <- paste0("ns(age_bl_c10, knots = c(",
                      # round(quantile(d$age_bl_c10, prob = c(0.25)),1), ",",
                      round(quantile(d$age_bl_c10, prob = c(0.50)),1), 
                      # ",",
                      # round(quantile(d$age_bl_c10, prob = c(0.75)),1),
                      "), Boundary.knots = c(",
                      round(quantile(d$age_bl_c10, prob = c(0.10)),1), ",",
                      round(quantile(d$age_bl_c10, prob = c(0.90)),1),"))")
    # run model
    md <- geeglm(as.formula(paste0("fnlim ~ child_forced_move*",
                                   time_ns, forms[[k]], agebl_ns)),
                     id = hhidpn, 
                     data = d,
                     family = gaussian(link="identity"),
                     corstr = "independence", # change per convo with Paloma 2/24/26
                     weights = d[[j]],
                     std.err = "san.se") ## Robust SEs
        
    pr <- geeglm(as.formula(paste0("fnlim_bin ~ child_forced_move*", 
                                   time_ns, forms[[k]], agebl_ns)),
                     id = hhidpn, 
                     data = d,
                     family = poisson(link="log"),
                     corstr = "independence",
                     weights = d[[j]],
                     std.err = "san.se") ## Robust SEs
        
    pd <- geeglm(as.formula(paste0("fnlim_bin ~ child_forced_move*", 
                                   time_ns, forms[[k]], agebl_ns)),
                     id = hhidpn, 
                     data = d,
                     family = poisson(link="identity"),
                     corstr = "independence",
                     weights = d[[j]],
                     std.err = "san.se", ## Robust SEs
                     mustart = pr$fitted.values) 
        
    mods <- list("md" = md, "pr" = pr, "pd" = pd)

      for(m in 1:length(mods)){
        
        list_tmp <- list(mods[[m]]) %>%
          setNames(nm = c(paste0(names(dats[i]), "_", j, "_", names(mods[m]))))
        all_models <- all_models %>% append(list_tmp)
   
        ## marginal predictions and contrasts -----
        
        ### Overall -- this uses the dataset-specific mean age_bl https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
        if(names(forms[k])== "intx2"){
          ps <- emmeans(mods[[m]], ~ child_forced_move | time_start10, 
                        at = list(time_start10 = seq(0, 2.5, by=.1)),
                        weights = "proportional") 
          
          cs <- emmeans::contrast(ps, "revpairwise")
        }
        else{
          ## 2/16/2026 TMM removing this for now because we are stratifying by bl age
          ### By baseine age (50,60,70)
          ps <- emmeans(mods[[m]], ~ child_forced_move | time_start10 | age_bl_c10,
                         at = list(time_start10 = seq(0, 2.5, by=.1),
                                   age_bl_c10 = c(-1.5, -0.5, 0.5)),
                        weights = "proportional")
          
          cs<- emmeans::contrast(ps, "revpairwise")
        }

        ps <- ps %>% as.data.frame() 
        cs <- cs %>% as.data.frame()
        
        if(!exists("preds")){
          assign("preds", ps %>%
                   mutate("sample" = names(dats[i]),
                          "weight" = j,
                          "formula" = names(forms[k]),
                          "model" = names(mods[m])))
          
          assign("contrasts", cs %>%
                   mutate("sample" = names(dats[i]),
                          "weight" = j,
                          "formula" = names(forms[k]),
                          "model" = names(mods[m])))
        } else{
          preds <- plyr::rbind.fill(preds, ps %>%
                                      mutate("sample" = names(dats[i]),
                                             "weight" = j,
                                             "formula" = names(forms[k]),
                                             "model" = names(mods[m])))
          
          contrasts <- plyr::rbind.fill(contrasts, cs %>%
                                          mutate("sample" = names(dats[i]),
                                                 "weight" = j,
                                                 "formula" = names(forms[k]),
                                                 "model" = names(mods[m])))
        }
      }
    }
  }
}

# Save -----
saveRDS(preds, here("Aim_2", "data", "processed", "preds_time.rds"))
saveRDS(contrasts, here("Aim_2", "data", "processed", "contrasts_time.rds"))
