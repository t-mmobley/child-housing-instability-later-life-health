# Purpose: R libraries to load
# Author: Taylor M. Mobley
# Date: Feb 5, 2025


# Read -----------------------------------------------------------
if (!require("pacman")){
  install.packages("pacman", repos = 'http://cran.us.r-project.org')
}
p_load("readr", "tidyverse", "janitor", "haven", "stringr", "lavaan", "mice",
       "lme4", "broom.mixed", "lmerTest", "ggplot2", "ggpubr", "VIM", "psych", 
       "gtsummary", "openxlsx", "readxl", "emmeans", "survival", "flextable",
       "officer", "visdat", "naniar", "twang", "nnet", "cobalt")
