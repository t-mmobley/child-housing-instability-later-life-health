# KM functions (edited from Paloma's Github repo)

# Note: modified from Paloma's 02_auxillary_fx.R script here:
# https://github.com/palolili23/2023_cancer_dementia/tree/main/02_R
risks_km <- function(model, ts) {
  
  risks <- NULL
  
  t <- model %>%
    summary(, times = ts, extend=TRUE)
  
  sum_fit <- data.frame(sample = names(dat_km[m]),
                            age = t$time, # to merge with the output below
                            exp = t$strata,
                            estimate = 1 - t$surv) %>%
    mutate(exp = case_when(exp == "child_forced_move=1" ~ 1,
                           exp == "child_forced_move=0" ~ 0)) %>%
    pivot_wider(
      names_from = exp,
      values_from = c(estimate),
      names_glue = "{.value}_{exp}"
      
    ) %>%
    mutate(
      rr = estimate_1 / estimate_0,
      rd = (estimate_1 - estimate_0)*100
    )

    if (length(risks) == 0){
      risks <- sum_fit
    } else {
      risks <- rbind(risks, sum_fit)
    }
  return(risks)
}

risks_cif <- function(model){
  model %>% 
    broom::tidy() %>%
    group_by(strata) %>% 
    slice(n()) %>% 
    mutate(estimate = 1 - estimate) %>% 
    select(estimate, strata) %>% 
    pivot_wider(names_from = strata, values_from = estimate) %>% 
    mutate(rd = .[[2]] - .[[1]],
           rr = .[[2]] / .[[1]]) %>% 
    mutate_at(c(1:3), ~.*100)
}

plot_cif <- function(model, plot.title){
  
  # Specs
  n <- as.numeric(sum(model$n))
  end <- ceiling(max(model$time) / 10) * 10
  strat <- length(model$strata)
  
  tidy <- model %>% 
    broom::tidy() %>% 
    select(time, strata, estimate, conf.high, conf.low) %>%
    rename(CIF = estimate) %>% 
    mutate(strata = gsub(".*=", "", strata)) %>%
    arrange(strata, time) %>% 
    group_by(strata) %>% 
    mutate(conf.low = ifelse(CIF == 0 & is.na(conf.low), 0, conf.low),
           conf.high = ifelse(CIF == 0 & is.na(conf.high), 0, conf.high)) %>%
    mutate(CIF = 1-CIF,
           conf.low = 1-conf.low,
           conf.high = 1-conf.high) %>%
    ungroup()
  
  
  plot <- tidy %>% 
    ggplot(aes(time, CIF, group = strata)) +
    geom_line(aes(color = strata), size = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
    labs(
      # title = paste0(plot.title, " (N = ", as.character(n), ")"),
      title = paste0(plot.title),
      color = NULL,
      y = "Cumulative mortality",
      x = "Age (years)") +
    theme_minimal() +
    theme(axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20),
          strip.text.x = element_text(size = 20),
          legend.text=element_text(size=20),
          plot.title = element_text(size = 20),
          legend.position="bottom",
          legend.title = element_blank()) +
    scale_x_continuous(breaks = seq(50, 110, by = 10), limits = c(50, 112)) +
    theme(legend.position = "bottom") 
  
  if (strat == 2) {
    plot <- plot + scale_color_manual(values = c("red3", "#56B4E9")) +
      scale_y_continuous(breaks = seq(0, 1, by = .25), limits = c(0, 1)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)
  } else {
    plot <- plot + 
      scale_color_manual(values = c("#8B0000", "#FFA07A", "#ADD8E6", "#00008B")) +
      scale_y_continuous(breaks = seq(0, 1, by = .25), limits = c(0, .75)) +
      guides(color = guide_legend(nrow = 2)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)
  }
  
  return(plot)
  
}

timeplot_cif <- function(model, plot.title){
  
  n <- as.numeric(sum(model$n))
  strat <- length(model$strata)
  
  tidy <- model %>% 
    broom::tidy() %>% 
    select(time, strata, estimate, conf.high, conf.low) %>%
    rename(CIF = estimate) %>% 
    arrange(strata, time) %>% 
    group_by(strata) %>% 
    mutate(strata = gsub(".*=", "", strata)) %>%
    mutate(conf.low = ifelse(CIF == 0 & is.na(conf.low), 0, conf.low),
           conf.high = ifelse(CIF == 0 & is.na(conf.high), 0, conf.high)) %>%
    mutate(CIF = 1-CIF,
           conf.low = 1-conf.low,
           conf.high = 1-conf.high) %>%
    ungroup()
  
  
  plot <- tidy %>% 
    ggplot(aes(time, CIF, group = strata)) +
    geom_line(aes(color = strata), size = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high) ,alpha = 0.2) +
    # scale_color_manual(values = c("red3", "#56B4E9")) +
    # scale_y_continuous(limits = c(0, 0.70)) +
    labs(
      # title = paste0(plot.title, " (N = ", as.character(n), ")"),
      title = paste0(plot.title),
      color = NULL,
      y = "Cumulative mortality",
      x = "Time on study (years)") +
    theme_minimal() +
    theme(axis.title.y = element_text(size=24),
          axis.text.x = element_text(size=24),
          axis.text.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          strip.text.x = element_text(size = 24),
          plot.title = element_text(size = 24),
          legend.text=element_text(size=24),
          legend.position="bottom",
          legend.title = element_blank()) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    theme(legend.position = "bottom") 
  
  if (strat == 2) {
    plot <- plot + scale_color_manual(values = c("red3", "#56B4E9")) +
      scale_y_continuous(breaks = seq(0, 1, by = .25), limits = c(0, 1)) +
      scale_x_continuous(breaks = seq(0, 25, by = 5), limits = c(0, 25)) 
  } else {
    plot <- plot + 
      scale_color_manual(values = c("#8B0000", "#FFA07A", "#ADD8E6", "#00008B")) +
      scale_y_continuous(breaks = seq(0, 1, by = .25), limits = c(0, .8)) +
      guides(color = guide_legend(nrow = 2)) +
      scale_x_continuous(breaks = seq(0, 8, by = 2), limits = c(0, 9)) 
  }
  
  return(plot)
  
}

# Baseline age-stratified bootstraps
bs_strata <- function(data) {
  # stratify on age, min to max every 5 increments
  age_strata <- seq(floor(min(data$age_start)), 
                    ceiling(max(data$age_start)) - 5, by = 5)
  strata <- lapply(1:(length(age_strata) - 1), function(i){ 
    df_strata <- data[data$age_start >= age_strata[i] & 
                        data$age_start < age_strata[i + 1], ] 
    df_sample <- sample(1:nrow(df_strata), nrow(df_strata), replace = T) 
    df_strata[df_sample, ]
    })
  
  df_last <- data[data$age_start >= last(age_strata), ]
  df_last_sample <- sample(1:nrow(df_last), size = nrow(df_last), replace = T)
  strata[[length(age_strata)]] <- data[df_last_sample, ]
  
  # combine together
  df_boot <- do.call(rbind, strata)
  return(df_boot)
}

# Note: modified from Paloma's 04c_bootstraps_ever.R script here:
# https://github.com/palolili23/2023_cancer_dementia/tree/main/02_R
risks_boots <- function(data, n, seed, exp, end, wt, ts, form){
  
  # Set seed
  set.seed(seed)
  
  # Creates bootsamples and runs the model to each sample
  bootsamps <- replicate(n = n, expr = {
    # random subset of data, with replacement
    d <- sample(1:nrow(data), size = nrow(data), replace = T)
    ds_b <- data[d, ]
    # ds_b <- bs_strata(data)
    
    # estimate IPTWs
    if (wt != "NULL") {
      # w <- ds_b[[wt]]
      
      ds_b$iptw_num <- predict(glm(child_forced_move ~ 1, 
                                            family=binomial(link=logit), 
                                            data=ds_b), ds_b, 
                                        type = "response")
      
      ds_b$iptw_den <- predict(glm(as.formula(form), 
                                            family=binomial(link=logit), 
                                            data=ds_b), ds_b, 
                                          type="response")
      
      w <- with(data=ds_b, ifelse(child_forced_move==1, 
                                   iptw_num/iptw_den,
                                   (1-iptw_num)/(1-iptw_den)))
    } else{
      w <- NULL
    }

    if (min(ts) >= 50) {
      temp <- survfit(as.formula(paste("Surv(age_start,", end, ", death_flag) ~ ", exp)),
                      weight = w,
                      data = ds_b)
    } else {
      temp <- survfit(as.formula(paste("Surv(", end, ", death_flag) ~ ", exp)),
                      weight = w,
                      data = ds_b)
    }
  }, simplify = F)
  
  totalboot <- NULL
  for (j in 1:n) {
    mod <- bootsamps[[j]]
    r <- risks_km(mod, ts)
    if (length(r) == 0){
      totalboot <- r
    } else {
      totalboot <- rbind(totalboot, r)
    }
  }
  
  totalboot <- totalboot %>%
    group_by(age) %>%
    mutate(across(.cols = c("estimate_0", "estimate_1", "rr", "rd"), ~quantile(.x, probs = c(0.025)),
                  .names = "{.col}_LL")) %>%
    mutate(across(.cols = c("estimate_0", "estimate_1", "rr", "rd"), ~quantile(.x, probs = c(0.975)),
                  .names = "{.col}_UL")) %>%
    mutate(across(.cols =c("estimate_0", "estimate_1", "rr", "rd"),
                  ~ quantile(.x, probs = c(0.5)),
                  .names = "{.col}_median")) %>%
    ungroup() %>%
    distinct(age, .keep_all = TRUE)
  
  return(totalboot)
}