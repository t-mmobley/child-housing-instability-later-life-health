# Purpose: Construct data set from RAND fat files that inclues residence, move,
#           MH variables in 1995-2020 waves
# Author: Taylor M. Mobley
# Date: Feb 11, 2024

library(tidyverse)
library(here)
library(haven)

full_directory_names <- 
  list.files(paste0("~/Library/CloudStorage/Box-Box/",
                    "HRS_MRG/RANDFatFiles"),
             full.names = TRUE) %>%
  str_subset(pattern = "^*\\.sas7bdat")

part_filenames <- 
  list.files(paste0("~/Library/CloudStorage/Box-Box/",
                    "HRS_MRG/RANDFatFiles"),
             full.names = FALSE) %>%
  str_subset(pattern = "^*\\.sas7bdat")

hrsfilenames <-
  lapply(X = part_filenames,
         FUN = stringi::stri_sub,
         to = -10)

names(full_directory_names) <- hrsfilenames

# Keeping 1995-2022 waves, when variables of interest were collected
# Note: may only collected in 1998-2022, and some only 2002-2022
print(full_directory_names)
full_directory_names$a95f2b <- NULL # old version of 1995 wave (I think)
full_directory_names$h98f2c <- NULL # old version of 1998 wave (I think)
full_directory_names$all_fat <- NULL # stacked data set with subset of vars
full_directory_names$sasfmts <- NULL # sas format data

hrsfatfiles <- lapply(FUN = read_sas, X = full_directory_names)
hrsfatfiles_clean <- list()

for (i in 1:length(hrsfatfiles)){
  colnames(hrsfatfiles[[i]]) <- tolower(colnames(hrsfatfiles[[i]]))
  hrsfatfiles_clean[[i]] <- hrsfatfiles[[i]] %>% 
    mutate(hrswave = full_directory_names[i],
           hhidpn = str_pad(hhidpn,9,pad="0")) %>%
    select(c(hhidpn, 
    # residential variables
             # 1998-2020 waves
             ## wH002 = home type; wH014 = owm mobile home/site
             ends_with("h002"), ends_with("h014"),
             ## wH004 = tenure; wX033 = whether moved past year
             ends_with("h004"), ends_with("X033"),
             ## wB039/wB040 = year/month moved to main res
             ends_with("b039"), ends_with("b040"), 
             ## wB041m1/m2 = two reasons for moves
             ends_with("b041m1"), ends_with("b041m2"),
             ## wH148 = rate home; wH149 = how easy make accessible
             ends_with("H148"), ends_with("H149"),
             ## rate safety of neighborhood
             ends_with("H050"),
             # 1995 and 1996 waves
             ## d2225/e2225 = type home
             ends_with("d2225"), ends_with("e2225"),
             ## tenure
             ends_with("d2226"), ends_with("e2226"),
             ## whether moved (1996 not 1995)
             ends_with("e88"),
             ## own mobile home/site
             ends_with("d2234"), ends_with("e2234"),
             ## d/e698 = year moved to main res
             ends_with("d698"), ends_with("e698"),
             ## d/e697 = month moved to main res
             ends_with("d697"), ends_with("e697"),
             ## d/e702m# = reasons for move (select all that apply in 95/96)
             contains("d702m"), contains("e702m"), 
             ## rate home
             ends_with("d2387"), ends_with("e2387"),
             ## make accessible
             ends_with("d2394"), ends_with("e2394"),
             ## rate safety
             ends_with("d2395"), ends_with("e2395"),
             # 1998 and 2000 waves
             ## type home
             ends_with("f2742"),ends_with("g3060"),
             ## tenure
             ends_with("f2743"), ends_with("g3061"),
             ## whether moved
             ends_with("f56"), ends_with("g56"),
             ## year moved to main res
             ends_with("f1017"), ends_with("g1104"),
             ## month moved to main res
             ends_with("f1018"),ends_with("g1105"),
             ## reasons for move 
             contains("f1022m"), contains("g1109m"),
             ## own mobile home/site
             ends_with("f2751"), ends_with("g3069")),
             ## rate home
             ends_with("f2904"), ends_with("g3222"),
             ## make accessible
             ends_with("f2911"), ends_with("g3229"),
             ## rate safety
             ends_with("f2912"), ends_with("g3230"),
             # family SES and forced moves
             ## 1998 and 2000
             ends_with("f993"), ends_with("f994"),
             ends_with("g1080"), ends_with("g1081"),
             ## 2002 - 2020
             ends_with("b020"), ends_with("b021"),
    # father unemployed during childhood
            ## 1998, 2000
            ends_with("f996"), ends_with("g1083"),
            ## 2002-2022
            ends_with("b023"),
    # father unemployed during childhood
            ## 2002-2022
            ends_with("b024m"))
}

# Assign wave names to list and sort before binding
names(hrsfatfiles_clean) <- names(hrsfatfiles)
hrsfatfiles_clean <- hrsfatfiles_clean[c("ad95f2b", "h96f4a", "hd98f2c",
                  "h00f1d", "h02f2c", "h04f1c", "h06f3a", "h08f3a", "hd10f5f",
                  "h12f3a", "h14f2b", "h16f2c", "h18f2b" , "h20e2a", "h22e3a")]

hrsfatfiles_join <- hrsfatfiles_clean %>% 
  reduce(full_join, by = "hhidpn")

saveRDS(hrsfatfiles_join, here("data", "processed", "hrsfatfiles_join.rds"))
