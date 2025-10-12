### --- About --- ###
### Project Title: SIEPR Data Task
### Script Name: douglas_kieran_code.R
### Author: Kieran Douglas
### Date: 10/12/2025
### Description: This is an empirical evaluation of the Small Business Training Program and its effect on firm productivity

### --- Package installation and loading data --- ###
install.packages(c("tidyverse", "car", "randomForest", "ivreg", "fixest", "sandwich", "lubridate"))
lapply(c("tidyverse", "car", "randomForest", "ivreg", "fixest", "sandwich", "lubridate"), library, character.only = TRUE)

# Change pathnames for reproducability.
firm_data <- read_csv("Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/firm_information.csv")
ag_sales <- read_csv("Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/aggregate_firm_sales.csv")
# Since the monthly data was provided across many .csv files, we need to merge them to one workable file.
monthly_files <- list.files(path = "/Users/kieran/Documents/MASTERS/METRICS/code/metrics/siepr_datatask/inputs/monthly_data", pattern = "\\.csv$", full.names = TRUE)
monthly_data <- map_dfr(monthly_files, read_csv)

### --- Data cleaning --- ###
# Due to inconsistencies in the day that each report is made, I am only going to care about year and month and create a new column for each df containing Y/m format.
ag_sales$year_month <- format(ymd(ag_sales$date), "%Y-%m")
# I noticed issues with dates in the monthly firm data where some were reported as Y/d/m rather than Y/m/d. To generate the new Y/m column I first need to standardize dates so it doesnt generate NA values by coersion.
monthly_data$date_standardized <- parse_date_time(monthly_data$date, orders = c("ymd", "ydm", "dmy", "mdy"))
monthly_data$year_month <- format(ymd(monthly_data$date_standardized), "%Y-%m")

# Now that I have a formatted Y/m dates column I want to merge each df by firm name and date
# Since only monthly_data and ag_sales have both firm id and date, I will conduct a two step merge
merged_step1 <- reduce(list(ag_sales, monthly_data), ~ left_join(.x, .y, by = c("firm_id", "year_month")))

# The merged df has additional rows which raises red flags, perhaps duplicates are present!
table(duplicated(ag_sales[, c("firm_id", "year_month")]))
table(duplicated(monthly_data[, c("firm_id", "year_month")]))

merged_step1 %>%
  group_by(firm_id, year_month) %>%
  filter(n() > 1) %>%
  distinct(firm_id, year_month)

# Duplicate rows are present for monthly_data and upon further inspection is looks like some firms reported multiple times in the same month. I am only interested in one report per month so I will keep the one that happened latest. 
merged_step1 <- merged_step1 %>%
  arrange(firm_id, year_month) %>%
  group_by(firm_id, year_month) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Now I am going to merge the firm_data df, including sector and name for each row.
merged_step2 <- merged_step1 %>%
  left_join(firm_data, by = "firm_id")

# Now that everything has been merged, I will conduct some general data cleaning to make everything usable. 
# I first want to decide what to do with missing data. The question of whether to impute or drop will be addressed by determining the nature of the missingness for each variable.  
# Sales
merged_step2$miss_sales_t <- is.na(merged_step2$sales_t)
glm_sales   <- glm(miss_sales_t ~ employment_t + wage_bill_t + revenue_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(glm_sales)
# Employment
merged_step2$miss_employment_t <- is.na(merged_step2$employment_t)
glm_employment   <- glm(miss_employment_t ~ sales_t + wage_bill_t + revenue_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(glm_employment)
# Wage
merged_step2$miss_wage_bill_t <- is.na(merged_step2$wage_bill_t)
miss_wage_bill_t   <- glm(miss_wage_bill_t ~ employment_t + sales_t + revenue_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(miss_wage_bill_t)
# Revenue
merged_step2$miss_revenue_t <- is.na(merged_step2$revenue_t)
glm_revenue   <- glm(miss_revenue_t ~ employment_t + wage_bill_t + sales_t + adopt_t + factor(firm_sector), 
                   data = merged_step2, family = binomial)
summary(glm_revenue)

# None of the regressions show statistically significant coeffs. This suggests that the probability of missingness is unrelated to the included observables. 
# This probably means that the NA values are missing completely at random (MCAR) and provides justification for dropping missing values, especially since I will still be left with a large number of observations (n=47696)
merged_clean <- merged_step2 %>% 
  na.omit(merged_step2) 
# I am interested in a Two Way Fixed Effects Difference in Differences estimation technique which relies on a few assumptions, one of which I am especially worried about. This is the assumption that treatment effects are homogenous. The fact that firms seem to join or opt out of the training program at staggered times is cause for concern.
# Because of this I will limit my analysis to firms who either adopted the training in 2013 (the first year the data shows people started using it) or those who never adopted it (as my control group)
adoption <- merged_clean %>%
  filter(adopt_t == 1) %>%
  group_by(firm_id) %>%
  summarize(first_adopt_month = min(year_month), .groups = "drop")

# Extract year from first_adopt_month.
adoption <- adoption %>%
  mutate(first_adopt_year = as.numeric(substr(first_adopt_month, 1, 4)))

# Rejoin merged_clean df. 
merged_clean <- merged_clean %>%
  left_join(adoption, by = "firm_id")

# Filter exclusively for either 2013 program adopters or never adopters
filtered_data <- merged_clean %>%
  filter(first_adopt_year == 2013 | is.na(first_adopt_year))

# At this point I have wrangled the data to exclude cases of staggered adoption, included leads for the parallel trends assumption, and merged the seperate df by simplified date and firm_id.
# The next step will be to refine the data, generate additional variables, and ensure readiness for analysis.
merged_clean <- filtered_data %>% 
  select(firm_id, sales_t, year_month, employment_t, wage_bill_t, revenue_t, adopt_t, firm_name, firm_sector) %>% 
  group_by(firm_id) %>%
  arrange(year_month, .by_group = TRUE) %>%
  mutate(
    rev_per_employee = revenue_t / employment_t,
    wagerev_ratio = wage_bill_t / revenue_t,
    salesgrowth = (sales_t - lag(sales_t)) / lag(sales_t),
    revgrowth = (revenue_t - lag(revenue_t)) / lag(revenue_t),
    rev_per_employee_change = (rev_per_employee-lag(rev_per_employee))/lag(rev_per_employee)
  ) %>%
  ungroup() 
  
# I will also create a firm performance index (despite having my qualms with their interpretability) for the sake of exploration and potential added rhobustness to my analysis. In this index, I only include change variables since firm size can paint a weird picture.
# The index normalizes included variables by z-score and shows the number of standard deviations the firm is from the mean.
index_vars <- c(
  "rev_per_employee_change", "salesgrowth", "revgrowth"
)
merged_clean <- merged_clean %>%
  mutate(across(all_of(index_vars), scale, .names = "{.col}_norm"))
merged_clean <- merged_clean %>%
  rowwise() %>%
  mutate(
    performance_index = mean(
      c_across(ends_with("_norm")),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

