# Purpose: IPTW, IPSWc, and IPSWd
# Author: Taylor M. Mobley
# Date: May 15, 2025

library(here)
library(twang)
library(nnet)
library(ipw)
library(survival)
library(cobalt)
library(patchwork)
# source(here("construction_code", "00_libraries.R"))

# Import ---------------------------------------------------
dat <- readRDS(here("Aim_2", "data", "processed", "dat_fnlim_long.rds"))
dat_cens <- readRDS(here("Aim_2", "data", "processed", "dat_fnlim_interval.rds"))

## checking median age = 59.41
t <- dat %>% group_by(hhidpn) %>% filter(agewave==min(agewave)) %>% ungroup()
quantile(t$agewave, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
quantile(t$age_bl, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))

dat_list <- list("dat_fnlim_all" = dat,
                 "dat_fnlim_insec" = dat %>% filter(poor_family_financ==1),
                 "dat_fnlim_sec" = dat %>% filter(poor_family_financ==0)
                 # "dat_fnlim_all_5060" = dat %>% filter(age_bl <=60),
                 # "dat_fnlim_insec_5060" = dat %>% filter(poor_family_financ==1) %>% filter(age_bl <=60),
                 # "dat_fnlim_sec_5060" = dat %>% filter(poor_family_financ==0) %>% filter(age_bl <=60),
                 # "dat_fnlim_all_over60" = dat %>% filter(age_bl > 60),
                 # "dat_fnlim_insec_over60" = dat %>% filter(poor_family_financ==1) %>% filter(age_bl > 60),
                 # "dat_fnlim_sec_over60" = dat %>% filter(poor_family_financ==0) %>% filter(age_bl > 60)
                 )

datbl_list <- list()

for(i in 1:length(dat_list)){
  
 datbl_list[[i]] <- dat_list[[i]] %>% group_by(hhidpn) %>%
    filter(hrswave == min(hrswave)) %>%
    ungroup()

  names(datbl_list[i]) <- c(names(dat_list[i]))
 }

# IPTWs ---------------------------------------------

for(i in 1:length(datbl_list)){
  
  ## IPTW numerator -----
  datbl_list[[i]]$IPTW_num <- predict(glm(child_forced_move ~ 1, 
                                        family=binomial(link=logit), 
                                        data=datbl_list[[i]]), datbl_list[[i]], 
                                    type = "response")
  
  ## Demo weights -----
  datbl_list[[i]]$demowt_den <- predict(glm(child_forced_move ~ 
                                        female + raceth4 + factor(rabcohort), 
                                        family=binomial(link=logit), 
                                        data=datbl_list[[i]]), datbl_list[[i]], 
                                    type="response")
  
  datbl_list[[i]]$demo_iptw <- with(data=datbl_list[[i]], 
                                  ifelse(child_forced_move==1, 
                                         IPTW_num/demowt_den,
                                         (1-IPTW_num)/(1-demowt_den)))
  
  ## All confounder weights ------
  datbl_list[[i]]$allwt_den <- predict(glm(child_forced_move ~ 
                                         female + raceth4 + feduc_le8 + 
                                         meduc_le8 + sbirth + factor(rabcohort), 
                                       family=binomial(link=logit), 
                                       data=datbl_list[[i]]), datbl_list[[i]], 
                                   type="response")
  
  datbl_list[[i]]$all_iptw <- with(data=datbl_list[[i]], 
                                   ifelse(child_forced_move==1, 
                                          IPTW_num/allwt_den, 
                                          (1-IPTW_num)/(1-allwt_den)))
}

## Check wt distriutions ------

sapply(datbl_list, function(x) summary(x$all_iptw))

test <- datbl_list[[1]]

datbl_list[[1]] %>%
  group_by(child_forced_move) %>%
  summarise(
    n = n(),
    ESS = (sum(all_iptw)^2) / sum(all_iptw^2),
    pct = round(ESS/n*100, 1)
  )

# IPCW and IPDW -----

# create list of datasets
cens_list <- list("dat_fnlim_all" = dat_cens,
                 "dat_fnlim_insec" = dat_cens %>% filter(poor_family_financ==1),
                 "dat_fnlim_sec" = dat_cens %>% filter(poor_family_financ==0))
                 # "dat_fnlim_all_5060" = dat_cens %>% filter(age_bl <=60),
                 # "dat_fnlim_insec_5060" = dat_cens %>% filter(poor_family_financ==1) %>% filter(age_bl <=60),
                 # "dat_fnlim_sec_5060" = dat_cens %>% filter(poor_family_financ==0) %>% filter(age_bl <=60),
                 # "dat_fnlim_all_over60" = dat_cens %>% filter(age_bl > 60),
                 # "dat_fnlim_insec_over60" = dat_cens %>% filter(poor_family_financ==1) %>% filter(age_bl > 60),
                 # "dat_fnlim_sec_over60" = dat_cens %>% filter(poor_family_financ==0) %>% filter(age_bl > 60))

for(i in 1:length(cens_list)){
  
  if(names(cens_list[i])=="dat_lhms_cc"){
    exp = "child_move_insec4"
  } else{
    exp = "child_forced_move"
  }
  # Time on study as the timescale -----
  ### IPCW -----
  #### numerator
  ltfu_time_num <- coxph(Surv(time_start, time_end, ltfu) ~ 
                           agewave,
                        data=cens_list[[i]])

  # Note: these are the same!
  cens_list[[i]]$ipcw_time_num <- predict(ltfu_time_num, type="survival", newdata=cens_list[[i]])
  # cens_list[[i]]$test <- exp(-predict(ltfu_time_num, type="expected"))
  # summary(cens_list[[1]]$ipcw_time_num)

  #### denominator
  ltfu_time_den <- coxph(as.formula(paste0("Surv(time_start, time_end, ltfu) ~
                           agewave + female + ", exp," + sbirth + rabcohort + 
                           meduc_le8 + feduc_le8 + raceth4 + raeduc3 + 
                           hhincome_pp + fnlim + heart + diab + strok + 
                           fairpoor_shlt + cesd_high")),
                        data=cens_list[[i]])

  cens_list[[i]]$ipcw_time_den <- predict(ltfu_time_den, type="survival", newdata=cens_list[[i]])

  #### weight
  cens_list[[i]] <- cens_list[[i]] %>% arrange(hhidpn, time_start) %>%
    group_by(hhidpn) %>%
    mutate(ipcw_time = cumprod(ipcw_time_num/ipcw_time_den),
           ipcw_time_v2 = cumprod(1/ipcw_time_den)) %>%
    ungroup()
  
  ### IPDW -----
  #### numerator
  death_time_num <- coxph(Surv(time_start, time_end, death) ~ 
                            agewave,
                         data=cens_list[[i]])

  cens_list[[i]]$ipdw_time_num <- predict(death_time_num, type="survival", newdata=cens_list[[i]])
  
  #### denominator
  death_time_den <- coxph(as.formula(paste0("Surv(time_start, time_end, death) ~
                           agewave + female + ", exp," + sbirth + rabcohort + 
                           meduc_le8 + feduc_le8 + raceth4 + raeduc3 + 
                           hhincome_pp + fnlim + heart + diab + strok + 
                           fairpoor_shlt + cesd_high")),
                         data=cens_list[[i]])
  
  cens_list[[i]]$ipdw_time_den <- predict(death_time_den, type="survival", newdata=cens_list[[i]])
  
  #### weight
  cens_list[[i]] <- cens_list[[i]] %>% arrange(hhidpn, time_start) %>%
    group_by(hhidpn) %>%
    mutate(ipdw_time = cumprod(ipdw_time_num/ipdw_time_den),
           ipdw_time_v2 = cumprod(1/ipdw_time_den)) %>%
    ungroup()
  
  ### IPCDW -----
  #### numerator
  both_time_num <- coxph(Surv(time_start, time_end, death_ltfu) ~ 
                           agewave,
                         data=cens_list[[i]])
  
  # Note: these are the same!
  cens_list[[i]]$ipcdw_time_num <- predict(both_time_num, type="survival", newdata=cens_list[[i]])
  # cens_list[[i]]$test <- exp(-predict(ltfu_time_num, type="expected"))
  # summary(cens_list[[1]]$ipcw_time_num)
  
  #### denominator
  both_time_den <- coxph(as.formula(paste0("Surv(time_start, time_end, death_ltfu) ~
                           agewave + female + ", exp," + sbirth + rabcohort + 
                           meduc_le8 + feduc_le8 + raceth4 + raeduc3 + 
                           hhincome_pp + fnlim + heart + diab + strok + 
                           fairpoor_shlt + cesd_high")),
                         data=cens_list[[i]])
  
  cens_list[[i]]$ipcdw_time_den <- predict(both_time_den, type="survival", newdata=cens_list[[i]])
  
  #### weight
  cens_list[[i]] <- cens_list[[i]] %>% arrange(hhidpn, time_start) %>%
    group_by(hhidpn) %>%
    mutate(ipcdw_time = cumprod(ipcdw_time_num/ipcdw_time_den),
           ipcdw_time_v2 = cumprod(1/ipcdw_time_den)) %>%
    ungroup()
  
  ### adding rounded time_end variable
  cens_list[[i]] <- cens_list[[i]] %>%
    mutate(time_r = round(time_end, 0)) %>%
    mutate(censored = ifelse(ltfu==1 | death==1, 1, 0))
}

## checks 
options(scipen=999)
sapply(cens_list, function(x) sum(is.na(x$ipcw_time)))
sapply(cens_list, function(x) sum(is.na(x$ipdw_time)))
sapply(cens_list, function(x) sum(is.na(x$ipcdw_time)))
sapply(cens_list, function(x) quantile(x$ipcw_time, probs=c(seq(0,.9,.1),.99,.999,1)))
sapply(cens_list, function(x) quantile(x$ipdw_time, probs=c(seq(0,.9,.1),.99,.999,1)))
sapply(cens_list, function(x) quantile(x$ipcdw_time, probs=c(seq(0,.9,.1),.99,.999,1)))
sapply(cens_list, function(x) quantile(x$ipcw_time_v2, probs=c(seq(0,.9,.1),.99,.999,1)))
sapply(cens_list, function(x) quantile(x$ipdw_time_v2, probs=c(seq(0,.9,.1),.99, .999,1)))

temp <- list()
for(i in 1:length(cens_list)){
  temp[[i]] <- cens_list[[i]] %>%
    mutate(across(c(ipcw_time, ipdw_time, ipcdw_time), ~ 
                    ifelse(.x > quantile(.x, probs=c(.999)), quantile(.x, probs=c(.999)),
                           ifelse(.x < quantile(.x, probs=c(.001)), quantile(.x, probs=c(.001)), .x)),
                  .names = "{.col}_trunc"))
}

# result_list <- lapply(temp, function(x) {
#   x %>%
#     arrange(hhidpn, time_r) %>%
#     group_by(time_r) %>%
#     dplyr::select(ipcw_time_trunc, ipdw_time_trunc) %>% 
#     mutate(across(everything(), list(
#       sum = ~sum(.x, na.rm = TRUE),
#       mean = ~mean(.x, na.rm = TRUE),
#       min = ~min(.x, na.rm = TRUE),
#       max = ~max(.x, na.rm = TRUE)
#     ))) %>%
#     distinct(time_r, .keep_all = TRUE) %>%
#     arrange(time_r)
# })
# 
# print(result_list[[1]], n=30)
# print(result_list[[2]], n=30)
# print(result_list[[3]], n=30)
# print(result_list[[4]], n=30)

# Data cleaning -----

## Add IPTWs to long data sets -----
for(i in 1:length(dat_list)){
  
  iptws <- datbl_list[[i]] %>% select(c(hhidpn, contains("iptw")))
  
  dat_list[[i]] <- dat_list[[i]] %>%
    left_join(iptws, by=c("hhidpn")) %>%
    arrange(hhidpn, hrswave) %>%
    group_by(hhidpn) %>%
    fill(c(contains("iptw")), .direction=c("downup")) %>%
    ungroup()
}

sapply(dat_list, function(x) summary(x$all_iptw))
sapply(dat_list, function(x) sum(is.na(x$all_iptw)))

## Add predictions to full tall dataset -----
temp <- list()
for(i in 1:length(dat_list)){
  
  # Rename time_end to time_start in cens_list
  temp[[i]] <- cens_list[[i]] %>%
    select(c(hhidpn, time_end, 
             # agewave,
             contains("ipcw"), contains("ipdw"), contains("ipcdw"))) %>%
    rename(time_start = time_end)
           # agewave = agewave_end) 
  
  # Merge weights to full tall dataset by id and time_start
  dat_list[[i]] <- dat_list[[i]] %>%
    left_join(temp[[i]], by=c("hhidpn", "time_start")) 
}

options(scipen=999)
sapply(dat_list, function(x) summary(x$ipcw_time))
sapply(dat_list, function(x) summary(x$ipdw_time))
sapply(dat_list, function(x) summary(x$ipcdw_time))

## Make weight = 1 at baseline for everyone -----

for(i in 1:length(dat_list)){
  
  dat_list[[i]] <- dat_list[[i]] %>%
    mutate(ipcdw_time = ifelse(is.na(ipcdw_time), 1, ipcdw_time)) %>%
    mutate(ipdw_time = ifelse(is.na(ipdw_time), 1, ipdw_time)) %>%
    mutate(ipcw_time = ifelse(is.na(ipcw_time), 1, ipcw_time))      
}

# Distributions -  need to truncate death weight and DC weight
sapply(dat_list, function(x) round(quantile(x$ipcw_time, probs=c(0, 0.001, 0.01, 0.25, 0.5, 0.75, 0.99, 0.999, 1)),6))
sapply(dat_list, function(x) round(quantile(x$ipdw_time, probs=c(0, 0.001, 0.01, 0.25, 0.5, 0.75, 0.99, 0.999, 1)),6))
sapply(dat_list, function(x) round(quantile(x$ipcdw_time, probs=c(0, 0.001, 0.01, 0.25, 0.5, 0.75, 0.99, 0.999, 1)),6))

## Final weights -----

for(i in 1:length(dat_list)){
  
  dat_list[[i]] <- dat_list[[i]] %>%
    ## Truncated weights -----
    mutate(across(c(ipcw_time, ipdw_time, ipcdw_time), ~ 
                    ifelse(.x > quantile(.x, probs=c(.99)), quantile(.x, probs=c(.99)),
                     ifelse(.x < quantile(.x, probs=c(.01)), quantile(.x, probs=c(.01)), .x)),
                  .names = "{.col}_trunc")) %>%
    ## final weights as IPTW*IPDCW -----
    mutate(all.weight.time = all_iptw*ipcw_time*ipdw_time_trunc,
           td.weight.time.trunc = all_iptw*ipdw_time_trunc,
           tc.weight.time.trunc = all_iptw*ipcw_time_trunc,
           all.weight.time.trunc = all_iptw*ipcw_time_trunc*ipdw_time_trunc,
           comb.weight.time.trunc = all_iptw*ipcdw_time_trunc) %>%
           # all.weight.age = all_iptw*ipdcw_age,
           # all.weight.age.trunc = all_iptw*ipdcw_age_trunc)
  ## Recenter/rescale time and age vars -----
  mutate(time_start10 = time_start/10,
         age_bl_c10 = (age_bl - 65)/10,
         agewave_c10 = (agewave - 65)/10)
}

sapply(dat_list, function(x) summary(x$ipcw_time_trunc))
sapply(dat_list, function(x) summary(x$ipdw_time_trunc))
sapply(dat_list, function(x) summary(x$ipcdw_time_trunc))

sapply(dat_list, function(x) summary(x$all.weight.time))
sapply(dat_list, function(x) summary(x$all.weight.time))
sapply(dat_list, function(x) summary(x$td.weight.time.trunc))
sapply(dat_list, function(x) summary(x$tc.weight.time.trunc))
sapply(dat_list, function(x) summary(x$all.weight.time.trunc))
sapply(dat_list, function(x) summary(x$comb.weight.time.trunc))

# Save ----------------------------------------------------------------

saveRDS(dat_list, here("Aim_2", "data", "processed", "dat_wgts.rds"))
saveRDS(cens_list, here("Aim_2", "data", "processed", "cens_list.rds"))
