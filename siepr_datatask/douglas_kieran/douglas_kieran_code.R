# --- About ---
# Project Title: SIEPR Data Task
# Script Name: douglas_kieran_code.R
# Author: Kieran Douglas
# Date: 10/12/2025
# Description: This is an empirical evaluation of the Small Business Training Program and its effect on firm productivity
# --- Package installation and loading data ---
install.packages(c("tidyverse", "car", "randomForest", "ivreg", "fixest", "sandwich"))
lapply(c("tidyverse", "car", "randomForest", "ivreg", "fixest", "sandwich"), library, character.only = TRUE)

# Change pathnames for reproducability
firm_data <- read_csv("Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/firm_information.csv")
ag_sales <- read_csv("Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/aggregate_firm_sales.csv")
# Since the monthly data was provided across many .csv files, we need to merge them to one workable file
monthly_files <- list.files(path = "/Users/kieran/Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/monthly_data", pattern = "\\.csv$", full.names = TRUE)
monthly_data <- map_dfr(monthly_files, read_csv)

# --- Data cleaning ---





















