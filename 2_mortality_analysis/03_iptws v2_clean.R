# Purpose: IPTWs
# Author: Taylor M. Mobley
# Date: May 15, 2025

library(here)
library(cobalt)
source(here("Aim_1/code/0a_libraries.R"))

# Import ---------------------------------------------------
dat <- readRDS(here("Aim_1", "data", "processed", "dat_surv_imp.rds"))

dat_list <- list("dat_surv" = dat,
                 "dat_surv_insec" = dat %>% filter(poor_family_financ==1),
                 "dat_surv_sec" = dat %>% filter(poor_family_financ==0),
                 "dat_surv_5060" = dat %>% filter(age_start_median=="50-60"),
                 "dat_surv_insec_5060" = dat %>% filter(poor_family_financ==1) %>% filter(age_start_median=="50-60"),
                 "dat_surv_sec_5060" = dat %>% filter(poor_family_financ==0) %>% filter(age_start_median=="50-60"),
                 "dat_surv_over60" = dat %>% filter(age_start_median=="> 60"),
                 "dat_surv_insec_over60" = dat %>% filter(poor_family_financ==1) %>% filter(age_start_median=="> 60"),
                 "dat_surv_sec_over60" = dat %>% filter(poor_family_financ==0) %>% filter(age_start_median=="> 60"),
                 "dat_surv_insec2" = dat %>% filter(poorvar_family_financ==1),
                 "dat_surv_insec2_5060" = dat %>% filter(poorvar_family_financ==1) %>% filter(age_start_median=="50-60"),
                 "dat_surv_insec2_over60" = dat %>% filter(poorvar_family_financ==1) %>% filter(age_start_median=="> 60"),
                 "dat_surv_sec2" = dat %>% filter(poorvar_family_financ==0),
                 "dat_surv_sec2_5060" = dat %>% filter(poorvar_family_financ==0) %>% filter(age_start_median=="50-60"),
                 "dat_surv_sec2_over60" = dat %>% filter(poorvar_family_financ==0) %>% filter(age_start_median=="> 60"),
                 "dat_surv_cc" = dat %>% filter(complete_flag==1),
                 "dat_surv_cc_5060" = dat %>% filter(complete_flag==1) %>% filter(age_start_median=="50-60"),
                 "dat_surv_cc_over60" = dat %>% filter(complete_flag==1) %>% filter(age_start_median=="> 60"))

# Weights ---------------------------------------------

for(i in 1:length(dat_list)){
  
  ## Demo weights -----
  dat_list[[i]]$iptw_num <- predict(glm(child_forced_move ~ 1, 
                                        family=binomial(link=logit), 
                                        data=dat_list[[i]]), dat_list[[i]], 
                                    type = "response")
  
  ### without baseline age -----
  dat_list[[i]]$demowt_den <- predict(glm(child_forced_move ~ 
                                            female + raceth + factor(rabcohort), 
                                        family=binomial(link=logit), 
                                        data=dat_list[[i]]), dat_list[[i]], 
                                    type="response")
  
  dat_list[[i]]$demo_iptw <- with(data=dat_list[[i]], 
                                  ifelse(child_forced_move==1, 
                                         iptw_num/demowt_den,
                                         (1-iptw_num)/(1-demowt_den)))
  
  ### with baseline age -----
  dat_list[[i]]$demowt_agebl_den <- predict(glm(child_forced_move ~ 
                                                  female + raceth + 
                                                  age_start + factor(rabcohort), 
                                          family=binomial(link=logit), 
                                          data=dat_list[[i]]), dat_list[[i]], 
                                      type="response")
  
  dat_list[[i]]$demo_agebl_iptw <- with(data=dat_list[[i]], 
                                        ifelse(child_forced_move==1, 
                                               iptw_num/demowt_agebl_den, 
                                               (1-iptw_num)/(1-demowt_agebl_den)))
  
  ## All confounder weights ------

  ### without baseline age -----
  dat_list[[i]]$allwt_den <- predict(glm(child_forced_move ~ 
                                         female + raceth + feduc_le8 + 
                                         meduc_le8 + sbirth + factor(rabcohort), 
                                       family=binomial(link=logit), 
                                       data=dat_list[[i]]), dat_list[[i]], 
                                   type="response")
  
  dat_list[[i]]$all_iptw <- with(data=dat_list[[i]], ifelse(child_forced_move==1, 
                                                        iptw_num/allwt_den, 
                                                        (1-iptw_num)/(1-allwt_den)))
  
  ### with baseline age -----
  dat_list[[i]]$allwt_agebl_den <- predict(glm(child_forced_move ~ 
                                           female + raceth + feduc_le8 + 
                                           meduc_le8 + sbirth + age_start + factor(rabcohort), 
                                         family=binomial(link=logit), 
                                         data=dat_list[[i]]), dat_list[[i]], 
                                     type="response")
  
  dat_list[[i]]$all_agebl_iptw <- with(data=dat_list[[i]], 
                                       ifelse(child_forced_move==1, 
                                              iptw_num/allwt_agebl_den, 
                                              (1-iptw_num)/(1-allwt_agebl_den)))
}

# check wt component distributions -----
summary(dat_list[["dat_surv"]]$iptw_num)
summary(dat_list[["dat_surv"]]$demowt_den)
summary(dat_list[["dat_surv"]]$allwt_den)
summary(dat_list[["dat_surv"]]$demowt_agebl_den)
summary(dat_list[["dat_surv"]]$allwt_agebl_den)

# Check iptw distriutions ------

summary(dat_list[["dat_surv"]]$demo_iptw)
summary(dat_list[["dat_surv"]]$all_iptw)

summary(dat_list[["dat_surv"]]$demo_agebl_iptw)
summary(dat_list[["dat_surv"]]$all_agebl_iptw)

set.cobalt.options(binary = "std",
                   continuous = "std")

# List of cov balance plots -----
plot_list <- lapply(names(dat_list), function(nm) {
  
  dat <- dat_list[[nm]]
  
  bal <- bal.tab(
    child_forced_move ~ 
      female + raceth + feduc_le8 + 
      meduc_le8 + sbirth + age_start + factor(rabcohort),
    data = dat,
    weights = dat$all_agebl_iptw,
    method = "weighting",
    un = TRUE,
    s.d.denom = "pooled"
  )
  
  love.plot(
    bal,
    stats = "mean.diffs",
    abs = TRUE,
    threshold = 0.1,
    var.order="unadjusted",
    sample.names = c("Original", "Weighted"),
    title = paste("Covariate Balance -", nm)
  )
})

names(plot_list) <- names(dat_list)

plot_list

# Save ----------------------------------------------------------------

saveRDS(dat_list, here("Aim_1", "data", "processed", "dat_surv_wgts.rds"))